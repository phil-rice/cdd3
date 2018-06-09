package one.xingyi.cddengine

import one.xingyi.cddscenario.{Scenario, ScenarioLogic}
import one.xingyi.cddutilities.Arrows._
import one.xingyi.cddutilities.{LeftRightTree, Lens}

import scala.util.{Failure, Success, Try}


sealed trait DecisionTreeNode[P, R] {
  def logic: ScenarioLogic[P, R]
  def findLens(p: P): Lens[DecisionTreeNode[P, R], DecisionTreeNode[P, R]]
  def findLensAndCnLens = use(findLens)(lens => (lens, lens andThen DecisionTreeNode.nodeToConcL))
}


case class ConclusionNode[P, R](scenarios: List[Scenario[P, R]], logic: ScenarioLogic[P, R]) extends DecisionTreeNode[P, R] {
  def accept(s: Scenario[P, R]) = logic.accept(s)
  override def findLens(p: P): Lens[DecisionTreeNode[P, R], DecisionTreeNode[P, R]] = Lens.identity
}

case class DecisionNode[P, R](logic: ScenarioLogic[P, R], left: DecisionTreeNode[P, R], right: DecisionTreeNode[P, R]) extends DecisionTreeNode[P, R] {
  import DecisionTreeNode._
  override def findLens(p: P): Lens[DecisionTreeNode[P, R], DecisionTreeNode[P, R]] = logic.fn.isDefinedAt(p) match {
    case true => nodeToDNL andThen DNLtoRight[P, R] andThen right.findLens(p)
    case false => nodeToDNL andThen DNLtoLeft[P, R] andThen left.findLens(p)
  }
}

class DefaultNotSpecifiedException[P](p: P) extends RuntimeException(s"Default has not been specified")

object DecisionTreeNode {
  def defaultFn[P, R]: P => R = { p: P => throw new DefaultNotSpecifiedException(p) }
  def nodeToDNL[P, R] = Lens.cast[DecisionTreeNode[P, R], DecisionNode[P, R]]
  def nodeToConcL[P, R] = Lens.cast[DecisionTreeNode[P, R], ConclusionNode[P, R]]
  def DNLtoLeft[P, R] = Lens[DecisionNode[P, R], DecisionTreeNode[P, R]](_.left, (dn, c) => dn.copy(left = c))
  def DNLtoRight[P, R] = Lens[DecisionNode[P, R], DecisionTreeNode[P, R]](_.right, (dn, c) => dn.copy(right = c))

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

