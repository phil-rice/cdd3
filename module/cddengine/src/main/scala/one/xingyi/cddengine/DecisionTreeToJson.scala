package one.xingyi.cddengine
import java.io.File
import java.text.MessageFormat

import one.xingyi.cddscenario.Scenario
import one.xingyi.cddutilities._
import one.xingyi.cddutilities.json._


object DecisionTreePrinter {
  def toJson[J](implicit jsonWriter: JsonWriter[J]): DecisionTreePrinter[J] = new DecisionTreePrinter[J] {
    override def apply[P, R](decisionTree: DecisionTree[P, R])(implicit decisionTreeToJson: DecisionTreeToJson[P, R]): String =
      jsonWriter(decisionTreeToJson(decisionTree))
  }

  def toHtml[J: JsonWriter](implicit toHtmlForMaps: ToHtmlAndJson[JsonMaps]): DecisionTreePrinter[J] = new DecisionTreePrinter[J] {
    override def apply[P, R](decisionTree: DecisionTree[P, R])(implicit decisionTreeToJson: DecisionTreeToJson[P, R]): String = {
      val jsonValue = decisionTreeToJson(decisionTree)
      val json: String = implicitly[JsonWriter[J]] apply (jsonValue)
      val maps = JsonMaps(jsonValue)
      println(s"Maps: $maps")
      toHtmlForMaps(json, maps)
    }
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
  override def apply(tree: DecisionTree[P, R]): JsonValue =
    DecisionTreeNode.makeLrt(tree.root).map { tree => printer(tree.tree, tree.parents) }.fold[JsonObject](_.tree, (n, l, r) => JsonObject((n.nameAndValues ++ Seq("ifFalse" -> l, "ifTrue" -> r)): _*))
}

trait DecisionNodeAndParentsToJson[P, R] {
  def apply(node: DecisionTreeNode[P, R], parents: List[DecisionNode[P, R]]): JsonObject
}

object DecisionNodeAndParentsToJson {
  implicit def default[P: ShortPrint, R: ShortPrint] = new SimpleDecisionNodeAndParentsToJson[P, R]()
}

class SimpleDecisionNodeAndParentsToJson[P, R: ShortPrint](implicit shortPrintP: ShortPrint[P], shortPrintR: ShortPrint[R]) extends DecisionNodeAndParentsToJson[P, R] {

  import one.xingyi.cddutilities.json.JsonWriterLangauge._
  def definedAt[T](t: T)(implicit hasDefinedAt: IsDefinedInSourceCodeAt[T]): (String, JsonValue) = ("defined", hasDefinedAt(t).toString)
  def apply(node: DecisionTreeNode[P, R], parents: List[DecisionNode[P, R]]) = nodeToTuple(node)
  def conclusionNode(c: ConclusionNode[P, R]) = JsonObject("conclusionNode" -> JsonObject("scenarios" -> JsonList(c.scenarios.map(scenario)), definedAt(c.logic)))
  def decisionNode(d: DecisionNode[P, R]) = JsonObject("decisionNode" -> JsonObject("condition" -> toJsonString(d.logic.ifString), definedAt(d.logic)))
  def scenario(s: Scenario[P, R]): JsonValue = JsonObject("situation" -> toJsonString(shortPrintP(s.situation)), definedAt(s))

  def nodeToTuple: PartialFunction[DecisionTreeNode[P, R], JsonObject] = {
    case c: ConclusionNode[P, R] => conclusionNode(c)
    case d: DecisionNode[P, R] => decisionNode(d)
  }
}

object DecisionTreeTracer {

  def trace[J: JsonWriter, P, R](fileNamePattern: String)(s: List[Scenario[P, R]])(implicit toHtmlForMaps: ToHtmlAndJson[JsonMaps]) = {
    val list = DecisionTreeFolder.trace(s)
    def file(i: Any) = new File(MessageFormat.format(fileNamePattern, i.toString))
    list.zipWithIndex.foreach { case (traceData, i) => Files.printToFile(file(i)) { pw => pw.write(DecisionTreePrinter.toHtml apply traceData.tree) } }
    val index = list.zipWithIndex.collect { case (TraceData(tree, Some(s), st), i) => s"<a href=${file(i).toURI.getPath}>${s.logic.definedInSourceCodeAt} ${st.getClass.getSimpleName} ${s.situation}</a>" }.mkString("<br />\n")
    Files.printToFile(file("index"))(_.print(index))
  }
}