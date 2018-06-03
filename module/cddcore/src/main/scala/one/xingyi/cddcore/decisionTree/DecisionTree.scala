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

trait DecisionTreeFolder[P, R] extends ((DecisionTree[P, R], Scenario[P, R]) => DecisionTree[P, R])

object DecisionTreeFolder {
  implicit def folder[P, R]: DecisionTreeFolder[P, R] = { (tree, s) =>
    val lens = tree.findLens(s.situation)
    val lensCn = lens andThen DecisionTree.nodeToConcL
    val cn = lensCn(tree)
    SituationState(cn, s) match {
      case Empty | Compatible => lensCn.set(tree, cn.copy(scenarios = cn.scenarios :+ s))
    }
  }
}


object SituationStateStrategy {
}
object SituationState {
  type SituationStateStrategy[P, R] = PartialFunction[(ConclusionNode[P, R], Scenario[P, R]), SituationState]
  def apply[P, R](conclusionNode: ConclusionNode[P, R], s: Scenario[P, R]): SituationState = {
    val result = conclusionNode.fn(s.situation)
    val situationOkWithExistingNode = s.acceptResult(s.situation, result)
    (conclusionNode, s) match {
      case (c, _) if c.scenarios.isEmpty => Empty
      case (c, s) if situationOkWithExistingNode => Compatible
      case _ => Split
    }
  }
}
sealed trait SituationState
case object Empty extends SituationState
case object Compatible extends SituationState
case object Split extends SituationState