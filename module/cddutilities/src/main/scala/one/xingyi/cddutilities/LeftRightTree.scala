package one.xingyi.cddutilities
import one.xingyi.cddutilities.SemiGroupLanguage._

case class LeftRightTree[TP, T](tree: T, parents: List[TP], children: Option[(LeftRightTree[TP, T], LeftRightTree[TP, T])]) {
  def map[T1](fn: LeftRightTree[TP, T] => T1): LeftRightTree[TP, T1] = LeftRightTree(fn(this), parents, children.map { case (l, r) => (l.map(fn), r.map(fn)) })
  def fold[Acc](leafFn: LeftRightTree[TP, T] => Acc, composer: (Acc, Acc, Acc) => Acc)(implicit semiGroup: SemiGroup[Acc]): Acc = {
    val thisAcc: Acc = leafFn(this)
    children.fold(thisAcc) { case (l, r) => composer(thisAcc, l.fold[Acc](leafFn, composer), r.fold[Acc](leafFn, composer)) }
  }
}
object LeftRightTree {
}
