package one.xingyi.cddcore
import javax.swing.JPopupMenu.Separator
import one.xingyi.cddutilities.{IndentAnd, Monoid, ShortPrint, Strings}

trait EnginePrinter {
  //The first node is 'this node' the last node is the root of the tree
  def apply[P: ShortPrint, R: ShortPrint](node: DecisionTreeNode[P, R], parents: List[DecisionTreeNode[P, R]]): String
}

object EnginePrinter {
  def apply[P: ShortPrint, R: ShortPrint](dn: DecisionTreeNode[P, R])(implicit printer: EnginePrinter): String =
    LeftRightTree(dn).fold(lrt => printer(lrt.t, lrt.parents), lrt => Strings.blanks(lrt.parents.size-1) + " else\n")
  implicit def defaultPrinter(implicit pIndent: ShortPrint[IndentAnd[String]]): EnginePrinter = new EnginePrinter {
    import one.xingyi.cddutilities.DefinedInSourceCodeAtLanguage._
    override def apply[P: ShortPrint, R: ShortPrint](node: DecisionTreeNode[P, R], parents: List[DecisionTreeNode[P, R]]) =
      node match {
        case DecisionNode(logic, left, right) => pIndent(IndentAnd(parents.size, s"dt(${node.logic.ifString} ${node.logic.definedInSourceCodeAt}\n"))
        case ConclusionNode(scenarios, logic) => pIndent(IndentAnd(parents.size, s"cn(${node.logic.definedInSourceCodeAt} + ${ scenarios.map(s =>
          s.data.definedInSourceCodeAt +"/" +s.situation +"=>" + s.logic.result)}\n"))
      }
  }
}
case class LeftRightTree[TP, T](t: T, parents: List[TP], children: Option[(LeftRightTree[TP, T], LeftRightTree[TP, T])]) {
  def map[T1](fn: LeftRightTree[TP, T] => T1): LeftRightTree[TP, T1] = LeftRightTree(fn(this), parents, children.map { case (l, r) => (l.map(fn), r.map(fn)) })
  def fold[Acc](fn: LeftRightTree[TP, T] => Acc, separator: LeftRightTree[TP,T] => Acc)(implicit monoid: Monoid[Acc]): Acc = {
    import one.xingyi.cddutilities.MonoidLanguage._
    val thisAcc: Acc = fn(this)
    children.fold(thisAcc) { case (l, r) => thisAcc |+| l.fold[Acc](fn, separator) |+| separator(this) |+| r.fold[Acc](fn, separator) }

  }
}

object LeftRightTree {
  def apply[P, R](dn: DecisionTreeNode[P, R], parents: List[DecisionTreeNode[P, R]] = List()): LeftRightTree[DecisionTreeNode[P, R], DecisionTreeNode[P, R]] = dn match {
    case DecisionNode(_, left, right) => LeftRightTree[DecisionTreeNode[P, R], DecisionTreeNode[P, R]](dn, parents, Some((apply(left, dn :: parents), apply(right, dn :: parents))))
    case c: ConclusionNode[P, R] => LeftRightTree(c, parents, None)
  }
}
