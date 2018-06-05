package one.xingyi.cddcore

import one.xingyi.cddutilities.Arrows._
import one.xingyi.cddutilities.Lens

import scala.util.{Success, Try}

sealed trait DecisionTree[P, R] {
  def findLens(p: P): Lens[DecisionTree[P, R], DecisionTree[P, R]]
  def findLensAndCnLens = use(findLens)(lens => (lens, lens andThen DecisionTree.nodeToConcL))
}


case class ConclusionNode[P, R](scenarios: List[Scenario[P, R]], logic: ScenarioLogic[P, R]) extends DecisionTree[P, R] {
  override def findLens(p: P): Lens[DecisionTree[P, R], DecisionTree[P, R]] = Lens.identity
}

case class DecisionNode[P, R](condition: ScenarioLogic[P, R], left: DecisionTree[P, R], right: DecisionTree[P, R]) extends DecisionTree[P, R] {
  import DecisionTree._
  override def findLens(p: P): Lens[DecisionTree[P, R], DecisionTree[P, R]] = condition.isDefinedAt(p) match {
    case true => nodeToDNL andThen DNLtoRight[P, R] andThen right.findLens(p)
    case false => nodeToDNL andThen DNLtoLeft[P, R] andThen left.findLens(p)
  }
}

class DefaultNotSpecifiedException[P](p: P) extends RuntimeException(s"Default has not been specified")

object DecisionTree {
  def defaultFn[P, R]: P => R = { p: P => throw new DefaultNotSpecifiedException(p) }
  def nodeToDNL[P, R] = Lens.cast[DecisionTree[P, R], DecisionNode[P, R]]
  def nodeToConcL[P, R] = Lens.cast[DecisionTree[P, R], ConclusionNode[P, R]]
  def DNLtoLeft[P, R] = Lens[DecisionNode[P, R], DecisionTree[P, R]](_.left, (dn, c) => dn.copy(left = c))
  def DNLtoRight[P, R] = Lens[DecisionNode[P, R], DecisionTree[P, R]](_.right, (dn, c) => dn.copy(right = c))
}


trait DecisionTreeFolder[P, R] extends ((DecisionTree[P, R], Scenario[P, R]) => DecisionTree[P, R])

trait DefaultFunction[P, R] extends PartialFunction[P, R]

case class SimpleDefaultFunction[P, R](fn: P => R) extends DefaultFunction[P, R] {
  override def isDefinedAt(x: P): Boolean = true
  override def apply(v1: P): R = fn(v1)
}
object DecisionTreeFolder {
  implicit def folder[P, R](implicit conclusionNodeEditor: ConclusionNodeEditor[P, R], default: DefaultFunction[P, R]): DecisionTreeFolder[P, R] = { (tree, s) =>
    val (lens, lensCn) = tree.findLensAndCnLens(s.situation)
    lens.set(tree, conclusionNodeEditor.makeReplacementNode apply(lensCn(tree), s))
  }
  def apply[P, R](list: List[Scenario[P, R]])(implicit decisionTreeFolder: DecisionTreeFolder[P, R], default: DefaultFunction[P, R]): DecisionTree[P, R] =
    list.foldLeft[DecisionTree[P, R]](ConclusionNode(List(), ScenarioLogic.empty))(decisionTreeFolder)
}


object ScenarioSplitter {
  def create[P, R]: (ConclusionNode[P, R], Scenario[P, R]) => ScenarioSplitter[P, R] = (c, s) => new ScenarioSplitter(c, s)
}

case class ScenarioSplitter[P, R](conclusionNode: ConclusionNode[P, R], scenario: Scenario[P, R]) {
  def findFails(list: List[Scenario[P, R]])(code: P => R) = list.filterNot(s => Try(s.acceptResult(s.situation, code(s.situation))) == Success(true))

  val (sAccepts, sRejects) = (scenario :: conclusionNode.scenarios).partition(s => scenario.logic.isDefinedAt(s.situation))
  val (cAccepts, cRejects) = (scenario :: conclusionNode.scenarios).partition(s => conclusionNode.logic.isDefinedAt(s.situation))
  val emptyScenarios = List[Scenario[P, R]]()
  val rightFailsUsingScenarioCode = scenario.logic.code.fold(emptyScenarios)(findFails(sAccepts))
  val leftFailsUsingConditionCode = findFails(sRejects)(conclusionNode.logic.apply)
  val canUseScenarioAsCondition = rightFailsUsingScenarioCode.isEmpty && leftFailsUsingConditionCode.isEmpty
}

case class CannotAddScenarioException(scenarioSplitter: ScenarioSplitter[_, _]) extends Exception(scenarioSplitter.toString)


object ConclusionNodeEditor {
  implicit def defaultEditor[P, R] = new ConclusionNodeEditor[P, R]
}
class ConclusionNodeEditor[P, R] {
  type SS[P, R] = PartialFunction[(ConclusionNode[P, R], Scenario[P, R]), DecisionTree[P, R]]
  def addScenarioToConclusion(c: ConclusionNode[P, R], s: Scenario[P, R]): DecisionTree[P, R] = c.copy(scenarios = c.scenarios :+ s)
  def addScenarioToEmpty(c: ConclusionNode[P, R], s: Scenario[P, R]): DecisionTree[P, R] = c.copy(scenarios = c.scenarios :+ s, logic = s.logic)
  def makeDecisionNodeWithScenarioOnLeft(c: ConclusionNode[P, R], s: Scenario[P, R]) = DecisionNode(c.logic, ConclusionNode(List(s), s.logic), c)
  def makeDecisionNodeWithScenarioOnRight(c: ConclusionNode[P, R], s: Scenario[P, R])(implicit scenarioSplitter: ScenarioSplitter[P, R]) = {
    DecisionNode(s.logic, ConclusionNode(scenarioSplitter.sRejects, c.logic), ConclusionNode(scenarioSplitter.sAccepts, s.logic))
  }

  def empty: SS[P, R] = {
    case (c, s) if c.scenarios.isEmpty => addScenarioToEmpty(c, s)
  }
  def compatible: SS[P, R] = {
    case (c, s) if c.logic.isDefinedAt(s.situation) && s.acceptResult(s.situation, c.logic(s.situation)) => addScenarioToConclusion(c, s)
  }

  def compatibleButCanSpecifyLogicNow: SS[P, R] = {
    case (c, s) if !c.logic.hasCondition && s.logic.hasCondition && c.logic.isDefinedAt(s.situation) && s.acceptResult(s.situation, c.logic(s.situation)) => c.copy(scenarios = c.scenarios :+ s, logic = s.logic)
  }
  def compatibleButMustMergeCondition: SS[P, R] = {
    case (c, s) if s.logic.hasCondition && c.logic.hasCondition && c.logic.isDefinedAt(s.situation) && s.acceptResult(s.situation, c.logic(s.situation)) =>
      c.copy(scenarios = c.scenarios :+ s, logic = c.logic or s.logic)
  }

  def scenarioInLeft(implicit scenarioSplitter: ScenarioSplitter[P, R]): SS[P, R] = {
    case (c, s) if !c.logic.isDefinedAt(s.situation) => makeDecisionNodeWithScenarioOnLeft(c, s)
  }
  def scenarioInRight(implicit scenarioSplitter: ScenarioSplitter[P, R]): SS[P, R] = {
    case (c, s) if scenarioSplitter.canUseScenarioAsCondition => makeDecisionNodeWithScenarioOnRight(c, s)
  }
  def failIfCantDoIt(implicit scenarioSplitter: ScenarioSplitter[P, R]): SS[P, R] = {
    case _ => throw new CannotAddScenarioException(scenarioSplitter)
  }

  def makeReplacementNode: (ConclusionNode[P, R], Scenario[P, R]) => DecisionTree[P, R] =
    use2(ScenarioSplitter.create[P, R]) { implicit splitter =>
      empty orElse compatibleButCanSpecifyLogicNow orElse compatibleButMustMergeCondition orElse compatible orElse scenarioInLeft orElse scenarioInRight orElse failIfCantDoIt }

}
