package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario
import one.xingyi.cddutilities._
import one.xingyi.cddutilities.json.{JsonList, JsonObject, JsonValue, JsonWriter}


object DecisionTreePrinter {
  def toJson[J](implicit jsonWriter: JsonWriter[J]): DecisionTreePrinter[J] = new DecisionTreePrinter[J] {
    override def apply[P, R](decisionTree: DecisionTree[P, R])(implicit decisionTreeToJson: DecisionTreeToJson[P, R]): String =
      jsonWriter(decisionTreeToJson(decisionTree))
  }
  def toHtml[J: JsonWriter, P, R](tree: DecisionTree[P, R])(implicit decisionTreeToJson: DecisionTreeToJson[P, R], lrtToHtml: ToHtmlWithJson[LeftRightTree[DecisionNode[P, R], DecisionTreeNode[P, R]]]) =
    lrtToHtml(DecisionTreeNode.makeLrt(tree.root))(toJson.apply(tree))

}

trait DecisionTreePrinter[J] {
  def apply[P, R](decisionTree: DecisionTree[P, R])(implicit decisionTreeToJson: DecisionTreeToJson[P, R]): String
}

object DecisionTreeToJson {
  implicit def default[P, R](implicit printer: DecisionNodeAndParentsToJson[P, R]) = new SimpleDecisionTreeToJson[P, R]()
}
trait DecisionTreeToJson[P, R] extends (DecisionTree[P, R] => JsonValue)

class SimpleDecisionTreeToJson[P, R](implicit printer: DecisionNodeAndParentsToJson[P, R]) extends DecisionTreeToJson[P, R] {
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
