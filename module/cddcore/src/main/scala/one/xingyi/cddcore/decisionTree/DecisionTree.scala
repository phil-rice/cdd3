package one.xingyi.cddcore.decisionTree
import one.xingyi.cddcore.Scenario
import one.xingyi.cddutilities.Lens
import one.xingyi.cddutilities.Arrows._

sealed trait DecisionTree[P, R] {
  def findLens(p: P): Lens[DecisionTree[P, R], DecisionTree[P, R]]
  def findLensAndCnLens = use(findLens)(lens => (lens, lens andThen DecisionTree.nodeToConcL))
}

case class ConclusionNode[P, R](scenarios: List[Scenario[P, R]], fn: PartialFunction[P, R]) extends DecisionTree[P, R] {
  override def findLens(p: P): Lens[DecisionTree[P, R], DecisionTree[P, R]] = Lens.identity
}

case class DecisionNode[P, R](condition: P => Boolean, left: DecisionTree[P, R], right: DecisionTree[P, R]) extends DecisionTree[P, R] {
  import DecisionTree._
  override def findLens(p: P): Lens[DecisionTree[P, R], DecisionTree[P, R]] = condition(p) match {
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
object DecisionTreeFolder {
  implicit def folder[P, R](implicit conclusionNodeEditor: ConclusionNodeEditor[P, R]): DecisionTreeFolder[P, R] = { (tree, s) =>
  val (lens, lensCn) = tree.findLensAndCnLens(s.situation)
  lens.set(tree, conclusionNodeEditor.makeReplacementNode(lensCn(tree), s))
  }
  def apply[P, R](default: P => R)(list: List[Scenario[P, R]])(implicit decisionTreeFolder: DecisionTreeFolder[P, R]): DecisionTree[P, R] =
    list.foldLeft[DecisionTree[P, R]](ConclusionNode(List(), { case p => default(p) }))(decisionTreeFolder)
}


object ScenarioSplitter {
  def create[P, R]: (ConclusionNode[P, R], Scenario[P, R]) => ScenarioSplitter[P, R] = (c, s) => new ScenarioSplitter(c, s)
}

case class ScenarioSplitter[P, R](conclusionNode: ConclusionNode[P, R], scenario: Scenario[P, R]) {
  val (inS, outS) = (scenario :: conclusionNode.scenarios).partition(s => scenario.reason.isDefinedAt(s.situation))
  def findFails(list: List[Scenario[P, R]])(code: P => R) = list.filterNot(s => scenario.reason.isDefinedAt(s.situation) && s.acceptResult(s.situation, code(s.situation)))
  val emptyScenarios = List[Scenario[P, R]]()
  val rightFailsUsingScenarioCode = scenario.reason.code.fold(emptyScenarios)(findFails(outS))
  val leftFailsUsingSenarioCode = findFails(inS)(conclusionNode.fn.apply)
  val canUseScenario = rightFailsUsingScenarioCode.isEmpty && leftFailsUsingSenarioCode.isEmpty
}

case class CannotAddScenarioException(scenarioSplitter: ScenarioSplitter[_, _]) extends Exception(scenarioSplitter.toString)

class ConclusionNodeEditor[P, R](default: P => R) {
  type SS[P, R] = PartialFunction[(ConclusionNode[P, R], Scenario[P, R]), DecisionTree[P, R]]
  def addScenarioToConclusion(c: ConclusionNode[P, R], s: Scenario[P, R]): DecisionTree[P, R] = c.copy(scenarios = c.scenarios :+ s)
  def makeDecisionNodeWithScenarioOnLeft(c: ConclusionNode[P, R], s: Scenario[P, R]) = DecisionNode(c.fn.isDefinedAt, c, ConclusionNode(List(s), s.reason.partialFunction(default)))
  def makeDecisionNodeWithScenarioOnRight(c: ConclusionNode[P, R], s: Scenario[P, R])(implicit scenarioSplitter: ScenarioSplitter[P, R]) = {
    DecisionNode(s.reason.isDefinedAt, ConclusionNode(scenarioSplitter.outS, c.fn), ConclusionNode(scenarioSplitter.inS, s.reason.partialFunction(default)))
  }

  def empty: SS[P, R] = {case (c, s) if c.scenarios.isEmpty => addScenarioToConclusion(c, s)}
  def compatible: SS[P, R] = {case (c, s) if c.fn.isDefinedAt(s.situation) && s.acceptResult(s.situation, c.fn(s.situation)) => addScenarioToConclusion(c, s)}
  def scenarioInLeft: SS[P, R] = {case (c, s) if !c.fn.isDefinedAt(s.situation) => makeDecisionNodeWithScenarioOnLeft(c, s)}
  def scenarioInRight(implicit scenarioSplitter: ScenarioSplitter[P, R]): SS[P, R] = {case (c, s) if scenarioSplitter.canUseScenario => makeDecisionNodeWithScenarioOnRight(c, s)}
  def failIfCantDoIt(implicit scenarioSplitter: ScenarioSplitter[P, R]): SS[P, R] = {case _ => throw new CannotAddScenarioException(scenarioSplitter)}

  def makeReplacementNode: (ConclusionNode[P, R], Scenario[P, R]) => DecisionTree[P, R] =
    use2(ScenarioSplitter.create[P, R]) { implicit splitter => empty orElse compatible orElse scenarioInLeft orElse scenarioInRight orElse failIfCantDoIt }

}
