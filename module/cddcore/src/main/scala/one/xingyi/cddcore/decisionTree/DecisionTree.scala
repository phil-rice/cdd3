package one.xingyi.cddcore.decisionTree
import one.xingyi.cddcore.Scenario
import one.xingyi.cddutilities.Lens

sealed trait DecisionTree[P, R] {
  def findLens(p: P): Lens[DecisionTree[P, R], DecisionTree[P, R]]
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


object DecisionTreeFolder {
  implicit def folder[P, R](implicit conclusionNodeEditor: ConclusionNodeEditor[P, R]): DecisionTreeFolder[P, R] = { (tree, s) =>
  val lens = tree.findLens(s.situation)
  val lensCn = lens andThen DecisionTree.nodeToConcL
  val cn = lensCn(tree)
  val x: DecisionTree[P, R] = conclusionNodeEditor.find((cn, s))
  ???
  }
}


class SituationStateStrategy[P, R] {


}


class SituationSplitter[P, R](conclusionNode: ConclusionNode[P, R], scenario: Scenario[P, R]) {
  val (left, right) = conclusionNode.scenarios.partition(s => scenario.reason.isDefinedAt(s.situation))
  val reasonCode = scenario.reason.code
  val compatible = conclusionNode.fn.isDefinedAt(scenario.situation) && scenario.acceptResult(scenario.situation, conclusionNode.fn(scenario.situation))
  val canUseConclusionNode = !conclusionNode.fn.isDefinedAt(scenario.situation)
  val rightFailsUsingScenarioCode: List[Scenario[P, R]] = reasonCode.fold[List[Scenario[P, R]]](List())(code => right.filterNot(s => scenario.reason.isDefinedAt(s.situation) && s.acceptResult(s.situation, code(s.situation))))
  val rightOkUsingScenarioCode = rightFailsUsingScenarioCode.isEmpty
  val leftFailsUsingSenarioCode = reasonCode.fold[List[Scenario[P, R]]](List())(code => left.filterNot(s => conclusionNode.fn.isDefinedAt(s.situation) && s.acceptResult(s.situation, conclusionNode.fn(s.situation))))
  val leftOkUsingSenarioCode = leftFailsUsingSenarioCode.isEmpty
  val canUseScenario = reasonCode.isDefined && leftOkUsingSenarioCode && rightOkUsingScenarioCode

  //  if (conclusionNode.scenarios.isEmpty) Empty else if (compatible) Compatible else if (canUseConclusionNode) CanPlaceScenarioInLeft else if (canUseScenario) CanPlaceScenarioInRight

}

trait DecisionTreeFolder[P, R] extends ((DecisionTree[P, R], Scenario[P, R]) => DecisionTree[P, R])


trait ConclusionNodeEditor[P, R] {
  type SS[P, R] = PartialFunction[(ConclusionNode[P, R], Scenario[P, R]), DecisionTree[P, R]]
  def addScenarioToConclusion(c: ConclusionNode[P, R], s: Scenario[P, R]): DecisionTree[P, R] = c.copy(scenarios = c.scenarios :+ s)
  def makeDecisionNodeWithScenarioOnLeft(c: ConclusionNode[P, R], s: Scenario[P, R]) = DecisionNode(c.fn.isDefinedAt, c, ConclusionNode(List(s), s.reason.partialFn))
  def empty: SS[P, R] = {
    case (c, s) if c.scenarios.isEmpty => addScenarioToConclusion(c, s)
  }

  def compatible: SS[P, R] = {
    case (c, s) if c.fn.isDefinedAt(s.situation) && s.acceptResult(s.situation, c.fn(s.situation)) => addScenarioToConclusion(c, s)
  }

  def scenarioInLeft: SS[P, R] = {
    case (c, s) if !c.fn.isDefinedAt(s.situation) => makeDecisionNodeWithScenarioOnLeft(c, s)
  }
  def scenarioInRight : SS[P,R] ={
    case _ if false => ???
  }
  def find = empty orElse compatible orElse scenarioInLeft orElse scenarioInRight

}
//
//case class Empty[P, R]() extends SituationState[P, R] {
//  override def isDefinedAt(cn: (ConclusionNode[P, R], Scenario[P, R])): Boolean = cn._1.scenarios.isEmpty
//  override def apply(cn: (ConclusionNode[P, R], Scenario[P, R])): DecisionTree[P, R] = cn._1.copy(scenarios = cn._1.scenarios :+ cn._2)
//}
//
//case class Compatible[P, R]() extends SituationState[P, R] {
//  override def isDefinedAt(x: (ConclusionNode[P, R], Scenario[P, R])): Boolean = x match {
//    case (c, s) => c.fn.isDefinedAt(s.situation) && s.acceptResult(s.situation, c.fn(s.situation))
//  }
//  override def apply(cn: (ConclusionNode[P, R], Scenario[P, R])): DecisionTree[P, R] = cn._1.copy(scenarios = cn._1.scenarios :+ cn._2)
//}
//case class CanPlaceScenarioInLeft[P, R]() extends SituationState[P, R] {
//  override def isDefinedAt(x: (ConclusionNode[P, R], Scenario[P, R])): Boolean = x match {
//    case (c, s) => !c.fn.isDefinedAt(s.situation)
//  }
//  override def apply(cn: (ConclusionNode[P, R], Scenario[P, R])): DecisionTree[P, R] = DecisionNode(cn._1.fn.isDefinedAt, cn._1, ConclusionNode(List(cn._2), cn._2.reason.partialFn))
//}
//case class CanPlaceScenarioInRight[P, R]() extends SituationState[P, R]