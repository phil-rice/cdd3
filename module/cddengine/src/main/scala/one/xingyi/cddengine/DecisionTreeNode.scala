package one.xingyi.cddengine

import one.xingyi.cddscenario.{HasEngineComponentData, Scenario, ScenarioLogic}
import one.xingyi.cddutilities.Arrows._
import one.xingyi.cddutilities.{IsDefinedInSourceCodeAt, LeftRightTree, Lens}

import scala.util.{Failure, Success, Try}


sealed trait DecisionTreeNode[P, R] {
  def logic: ScenarioLogic[P, R]
  def findLens(p: P): Lens[DecisionTreeNode[P, R], DecisionTreeNode[P, R]]
  def findLensAndCnLens = use(findLens)(lens => (lens, lens andThen DecisionTreeNode.nodeToConcL))
}


object ConclusionNode {
  implicit def isDefined[P, R]: IsDefinedInSourceCodeAt[ConclusionNode[P, R]] = { c => c.logic.definedInSourceCodeAt }
}
case class ConclusionNode[P, R](scenarios: List[Scenario[P, R]], logic: ScenarioLogic[P, R]) extends DecisionTreeNode[P, R] {
  def accept(s: Scenario[P, R]) = logic.accept(s)
  override def findLens(p: P): Lens[DecisionTreeNode[P, R], DecisionTreeNode[P, R]] = Lens.identity
}

object DecisionNode {
  implicit def isDefined[P, R]: IsDefinedInSourceCodeAt[DecisionNode[P, R]] = { d => d.logic.definedInSourceCodeAt }
}
case class DecisionNode[P, R](logic: ScenarioLogic[P, R], ifFalse: DecisionTreeNode[P, R], ifTrue: DecisionTreeNode[P, R]) extends DecisionTreeNode[P, R] {
  import DecisionTreeNode._
  override def findLens(p: P): Lens[DecisionTreeNode[P, R], DecisionTreeNode[P, R]] = logic.fn.isDefinedAt(p) match {
    case true => nodeToDNL andThen DNLtoTrue[P, R] andThen ifTrue.findLens(p)
    case false => nodeToDNL andThen DNLtoFalse[P, R] andThen ifFalse.findLens(p)
  }
}

class DefaultNotSpecifiedException[P](p: P) extends RuntimeException(s"Default has not been specified")

object DecisionTreeNode {
  def defaultFn[P, R]: P => R = { p: P => throw new DefaultNotSpecifiedException(p) }
  def nodeToDNL[P, R] = Lens.cast[DecisionTreeNode[P, R], DecisionNode[P, R]]
  def nodeToConcL[P, R] = Lens.cast[DecisionTreeNode[P, R], ConclusionNode[P, R]]
  def DNLtoFalse[P, R] = Lens[DecisionNode[P, R], DecisionTreeNode[P, R]](_.ifFalse, (dn, c) => dn.copy(ifFalse = c))
  def DNLtoTrue[P, R] = Lens[DecisionNode[P, R], DecisionTreeNode[P, R]](_.ifTrue, (dn, c) => dn.copy(ifTrue = c))

  def makeLrt[P, R](node: DecisionTreeNode[P, R], parents: List[DecisionNode[P, R]] = List()): LeftRightTree[DecisionNode[P, R], DecisionTreeNode[P, R]] = node match {
    case d@DecisionNode(_, left, right) => LeftRightTree(node, parents, Some((makeLrt(left, d :: parents), makeLrt(right, d :: parents))))
    case c: ConclusionNode[P, R] => LeftRightTree(c, parents, None)
  }

}

sealed trait DecisionIssue[P, R] {
  def scenario: Scenario[P, R]
}
case class CannotAddScenarioBecauseClashes[P, R](scenario: Scenario[P, R], classesWith: List[Scenario[P, R]]) extends RuntimeException with DecisionIssue[P, R]

case class DecisionTree[P, R](root: DecisionTreeNode[P, R], issues: List[DecisionIssue[P, R]])
object DecisionTree {
  def empty[P, R]: DecisionTree[P, R] = DecisionTree(ConclusionNode(List(), ScenarioLogic.empty), List())
}


trait DefaultFunction[P, R] extends PartialFunction[P, R]

case class SimpleDefaultFunction[P, R](fn: P => R) extends DefaultFunction[P, R] {
  override def isDefinedAt(x: P): Boolean = true
  override def apply(v1: P): R = fn(v1)
}

