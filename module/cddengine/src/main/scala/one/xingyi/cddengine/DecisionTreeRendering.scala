package one.xingyi.cddengine
import java.io.File
import java.text.MessageFormat

import one.xingyi.cddscenario.{HasEngineComponentData, Scenario}
import one.xingyi.cddutilities.json.JsonLanguage._
import one.xingyi.cddutilities.json._
import one.xingyi.cddutilities.{Files, IsDefinedInSourceCodeAt, ShortPrint}

sealed abstract class NodeEffect(val json: String)
case object GoesThrough extends NodeEffect("goes_through")
case object WouldGoThrough extends NodeEffect("would_go_through")
case object Fails extends NodeEffect("fails")

//object DecisionTreeTracer {
//
//  def trace[J: JsonWriter, P, R](fileNamePattern: String)(s: List[Scenario[P, R]])(implicit toHtmlForMaps: ToHtmlAndJson[JsonMaps]) = {
//    val list = DecisionTreeFolder.trace(s)
//    def file(i: Any) = new File(MessageFormat.format(fileNamePattern, i.toString))
//    list.zipWithIndex.foreach { case (traceData, i) => Files.printToFile(file(i)) { pw => pw.write(DecisionTreePrinter.toHtml apply traceData.tree) } }
//    val index = list.zipWithIndex.collect { case (TraceData(tree, Some(s), st), i) => s"<a href=${file(i).toURI.getPath}>${s.logic.definedInSourceCodeAt} ${st.getClass.getSimpleName} ${s.situation}</a>" }.mkString("<br />\n")
//    Files.printToFile(file("index"))(_.print(index))
//  }
//}
object DecisionTreeRendering {
  def simple[P: ShortPrint, R: ShortPrint]: DecisionTreeRendering[JsonObject, P, R] = new SimpleDecisionTreeRendering[P, R]
  def withScenario[P: ShortPrint, R: ShortPrint](s: Scenario[P, R], nodes: List[DecisionTreeNode[P, R]]): DecisionTreeRendering[JsonObject, P, R] = new WithScenarioRendering[P, R](WithScenarioData(s, nodes))
  def trace[P, R](rendering: DecisionTreeRendering[String, P, R])(fileNamePattern: String)(s: List[Scenario[P, R]]) = {
    val list = DecisionTreeFolder.trace(s)
    def file(i: Any) = new File(MessageFormat.format(fileNamePattern, i.toString))
    list.zipWithIndex.foreach { case (traceData, i) => Files.printToFile(file(i)) { pw => pw.write(rendering.tree apply traceData.tree) } }
    val index = list.zipWithIndex.collect { case (TraceData(tree, Some(s), st), i) => s"<a href=${file(i).toURI.getPath}>${s.logic.definedInSourceCodeAt} ${st.getClass.getSimpleName} ${s.situation}</a>" }.mkString("<br />\n")
    Files.printToFile(file("index"))(_.print(index))
  }

}

//We could have broken this down to individual type classes, but the type signatures would be horrible as we need to be able to pass in custom renderers and there is one per entity
trait DecisionTreeRendering[J, P, R] {
  def engine: Engine[P, R] => J
  def tree: DecisionTree[P, R] => J
  def useCase: UseCase1[P, R] => J
  def scenario: Scenario[P, R] => J
  def node: DecisionTreeNode[P, R] => J = {case c: ConclusionNode[P, R] => conclusionNode(c); case d: DecisionNode[P, R] => decisionNode(d)}
  def decisionNode: DecisionNode[P, R] => J
  def conclusionNode: ConclusionNode[P, R] => J
  def issue: DecisionIssue[P, R] => J
  def andThen[J1](fn: J => J1): DecisionTreeRendering[J1, P, R] = new TransformingTreeRending(this, fn)
}

class TransformingTreeRending[J, J1, P, R](rendering: DecisionTreeRendering[J, P, R], transform: J => J1) extends DecisionTreeRendering[J1, P, R] {
  override def engine: Engine[P, R] => J1 = rendering.engine andThen transform
  override def tree: DecisionTree[P, R] => J1 = rendering.tree andThen { p => println("in println" + p); p } andThen transform
  override def useCase: UseCase1[P, R] => J1 = rendering.useCase andThen transform
  override def scenario: Scenario[P, R] => J1 = rendering.scenario andThen transform
  override def decisionNode: DecisionNode[P, R] => J1 = rendering.decisionNode andThen transform
  override def conclusionNode: ConclusionNode[P, R] => J1 = rendering.conclusionNode andThen transform
  override def issue: DecisionIssue[P, R] => J1 = rendering.issue andThen transform
}


import one.xingyi.cddutilities.AnyLanguage._
class SimpleDecisionTreeRendering[P, R](implicit shortPrintP: ShortPrint[P], shortPrintR: ShortPrint[R]) extends DecisionTreeRendering[JsonObject, P, R] {
  implicit class JsonObjectOps(j: JsonObject) {
    def addOpt(nameAndValues: (String, Option[String])*) = JsonObject((j.nameAndValues ++ nameAndValues.collect { case (name, Some(value)) => Seq(name -> (value: JsonValue)) }.flatten): _*)
    def data[T](t: T)(implicit getData: HasEngineComponentData[T]): JsonObject = using(getData(t)) { data => j |+| ("defined" -> data.definedInSourceCodeAt.toString) addOpt("title" -> data.title, "comment" -> data.comment) }
    def defined[T](t: T)(implicit isDefinedInSourceCodeAt: IsDefinedInSourceCodeAt[T]) = j |+| ("defined" -> isDefinedInSourceCodeAt(t).toString)
    def addOptList(nameAndList: (String, List[JsonObject])) = if (nameAndList._2.size > 0) j |+| (nameAndList._1 -> JsonList(nameAndList._2), ("has" + nameAndList._1) -> true) else j
  }
  override def engine = ???
  override def tree = tree => JsonObject("tree" -> node(tree.root)) addOptList ("issues" -> tree.issues.map(issue))
  override def useCase = u => JsonObject("useCase" -> JsonObject().data(u))
  override def decisionNode: DecisionNode[P, R] => JsonObject = d => JsonObject("decisionNode" -> JsonObject("condition" -> toJsonString(d.logic.ifString), "ifTrue" -> node(d.ifTrue), "ifFalse" -> node(d.ifFalse))).defined(d)
  override def conclusionNode: ConclusionNode[P, R] => JsonObject = c => JsonObject("conclusionNode" -> JsonObject("scenarios" -> JsonList(c.scenarios.map(scenario)))).defined(c)
  override def scenario: Scenario[P, R] => JsonObject = s => JsonObject("situation" -> shortPrintP(s.situation)).data(s)
  override def issue: DecisionIssue[P, R] => JsonObject = e => JsonObject("issue" -> e.toString)
}

case class WithScenarioData[P, R](s: Scenario[P, R], nodes: List[DecisionTreeNode[P, R]])

object NodeEffect {
  import one.xingyi.cddutilities.AnyLanguage._
  import one.xingyi.cddutilities.FunctionLanguage._
  type NodeEffectNodeFn[P, R] = WithScenarioData[P, R] => DecisionTreeNode[P, R] => Option[NodeEffect]

  def goesThroughNode[P, R]: NodeEffectNodeFn[P, R] = { withScenarioData => node => withScenarioData.nodes.find(_ == node).map(_ => GoesThrough) }
  def wouldGoThroughNode[P, R]: NodeEffectNodeFn[P, R] = { withScenarioData => node => node.logic.accept(withScenarioData.s).asOption(WouldGoThrough) }
  def apply[P, R](withScenarioData: WithScenarioData[P, R]): DecisionTreeNode[P, R] => NodeEffect = goesThroughNode[P, R] orElse wouldGoThroughNode orDefault Fails apply withScenarioData
}

class WithScenarioRendering[P, R](withScenarioData: WithScenarioData[P, R]) extends SimpleDecisionTreeRendering[P, R] {
  implicit class WithSituationJsonObjectOps[From <: DecisionTreeNode[P, R]](j: From => JsonObject) {
    def addNodeEffect: From => JsonObject = { from => j(from) |+| ("node" -> NodeEffect(withScenarioData)(from).json) }
  }
  override def decisionNode: DecisionNode[P, R] => JsonObject = super.decisionNode.addNodeEffect
  override def conclusionNode: ConclusionNode[P, R] => JsonObject = super.conclusionNode.addNodeEffect
}