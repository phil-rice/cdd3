package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario
import one.xingyi.cddutilities._
import one.xingyi.cddutilities.json.{JsonList, JsonObject, JsonValue, JsonWriter}


object DecisionTreePrinter {
  def printer[J](implicit jsonWriter: JsonWriter[J]): DecisionTreePrinter[J] = new DecisionTreePrinter[J] {
    override def apply[P, R](decisionTree: DecisionTree[P, R])(implicit decisionTreeToJson: DecisionTreeToJson[P, R]): String =
      jsonWriter(decisionTreeToJson(decisionTree))
  }
}

trait DecisionTreePrinter[J] {
  def apply[P, R](decisionTree: DecisionTree[P, R])(implicit decisionTreeToJson: DecisionTreeToJson[P, R]): String
}

object DecisionTreeToJson {
  implicit def default[P, R](implicit printer: DecisionNodeAndParentsToJson[P, R]) = new SimpleDecisionTreeToJson[P, R]()
}
trait DecisionTreeToJson[P, R] extends (DecisionTree[P, R] => JsonValue)

class SimpleDecisionTreeToJson[P, R](implicit printer: DecisionNodeAndParentsToJson[P, R]) extends DecisionTreeToJson[P, R] {
  import one.xingyi.cddutilities.json.JsonWriterLangauge._
  override def apply(tree: DecisionTree[P, R]): JsonValue =
    DecisionTreeNode.makeLrt(tree.root).map { tree => printer(tree.t, tree.parents) }.fold[JsonObject](_.t, (n, l, r) => JsonObject((n.nameAndValues ++ Seq("ifFalse" -> l, "ifTrue" -> r)): _*))
}

trait DecisionNodeAndParentsToJson[P, R] {
  def apply(node: DecisionTreeNode[P, R], parents: List[DecisionNode[P, R]]): JsonObject
}

object DecisionNodeAndParentsToJson {
  implicit def default[P: ShortPrint, R: ShortPrint] = new SimpleDecisionNodeAndParentsToJson[P, R]()
}

class SimpleDecisionNodeAndParentsToJson[P, R: ShortPrint](implicit shortPrintP: ShortPrint[P], shortPrintR: ShortPrint[R]) extends DecisionNodeAndParentsToJson[P, R] {
  import one.xingyi.cddutilities.json.JsonWriterLangauge._
  def apply(node: DecisionTreeNode[P, R], parents: List[DecisionNode[P, R]]) = nodeToTuple(node)
  def conclusionNode(c: ConclusionNode[P, R]) = JsonObject("scenarios" -> JsonList(c.scenarios.map(scenario)))
  def decisionNode(d: DecisionNode[P, R]) = JsonObject("condition" -> toJsonString(d.logic.ifString))
  def scenario(s: Scenario[P, R]): JsonValue = JsonObject("situation" -> toJsonString(shortPrintP(s.situation)))

  def nodeToTuple: PartialFunction[DecisionTreeNode[P, R], JsonObject] = {
    case c: ConclusionNode[P, R] => conclusionNode(c)
    case d: DecisionNode[P, R] => decisionNode(d)
  }
}


//
//object DecisionNodePrinter {
//  def apply[P: ShortPrint, R: ShortPrint](dn: DecisionTreeNode[P, R])(implicit printer: DecisionNodePrinter): JsonValue =
//    LeftRightTree(dn).fold(lrt => JsonObject("parents", JsonList(lrt.parents), lrt => Strings.blanks(lrt.parents.size - 1) + " else\n")
//
//
//  implicit def defaultPrinter(implicit pIndent: ShortPrint[IndentAnd[String]]): DecisionNodePrinter = new DecisionNodePrinter {
//    import one.xingyi.cddutilities.DefinedInSourceCodeAtLanguage._
//    override def apply[P: ShortPrint, R: ShortPrint](node: DecisionTreeNode[P, R], parents: List[DecisionTreeNode[P, R]]) =
//      node match {
//        case DecisionNode(logic, left, right) => pIndent(IndentAnd(parents.size, s"dt(${node.logic.ifString} ${node.logic.definedInSourceCodeAt}\n"))
//        case ConclusionNode(scenarios, logic) => pIndent(IndentAnd(parents.size, s"cn(${node.logic.definedInSourceCodeAt} + ${
//          scenarios.map(s =>
//            s.data.definedInSourceCodeAt + "/" + s.situation + "=>" + s.logic.result)
//        }\n"))
//      }
//  }
//}
//case class LeftRightTree[TP, T](t: T, parents: List[TP], children: Option[(LeftRightTree[TP, T], LeftRightTree[TP, T])]) {
//  def map[T1](fn: LeftRightTree[TP, T] => T1): LeftRightTree[TP, T1] = LeftRightTree(fn(this), parents, children.map { case (l, r) => (l.map(fn), r.map(fn)) })
//  def fold[Acc](fn: LeftRightTree[TP, T] => Acc, separator: LeftRightTree[TP, T] => Acc)(implicit monoid: Monoid[Acc]): Acc = {
//    import one.xingyi.cddutilities.MonoidLanguage._
//    val thisAcc: Acc = fn(this)
//    children.fold(thisAcc) { case (l, r) => thisAcc |+| l.fold[Acc](fn, separator) |+| separator(this) |+| r.fold[Acc](fn, separator) }
//
//  }
//}
//
//object LeftRightTree {
//  def apply[P, R](dn: DecisionTreeNode[P, R], parents: List[DecisionTreeNode[P, R]] = List()): LeftRightTree[DecisionTreeNode[P, R], DecisionTreeNode[P, R]] = dn match {
//    case DecisionNode(_, left, right) => LeftRightTree[DecisionTreeNode[P, R], DecisionTreeNode[P, R]](dn, parents, Some((apply(left, dn :: parents), apply(right, dn :: parents))))
//    case c: ConclusionNode[P, R] => LeftRightTree(c, parents, None)
//  }
//}
