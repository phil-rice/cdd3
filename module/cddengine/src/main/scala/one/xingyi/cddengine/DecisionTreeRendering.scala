package one.xingyi.cddengine
import one.xingyi.cddscenario.{HasEngineComponentData, Scenario}
import one.xingyi.cddutilities.{IsDefinedInSourceCodeAt, ShortPrint}
import one.xingyi.cddutilities.json.{JsonList, JsonObject, JsonValue, ToJson}
import one.xingyi.cddutilities.json.JsonLanguage._

sealed abstract class NodeEffect(val json: String)
case object GoesThrough extends NodeEffect("goes_through")
case object WouldGoThrough extends NodeEffect("would_go_through")
case object Fails extends NodeEffect("fails")


//We could have broken this down to individual type classes, but the type signatures would be horrible as we need to be able to pass in custom renderers and there is one per entity
trait DecisionTreeRendering[P, R] {
  def engine: Engine[P, R] => JsonObject
  def tree: DecisionTree[P, R] => JsonObject

  def useCase: UseCase1[P, R] => JsonObject

  def node: DecisionTreeNode[P, R] => JsonObject = {
    case c: ConclusionNode[P, R] => conclusionNode(c)
    case d: DecisionNode[P, R] => decisionNode(d)
  }
  def decisionNode: DecisionNode[P, R] => JsonObject
  def conclusionNode: ConclusionNode[P, R] => JsonObject
  def scenario: Scenario[P, R] => JsonObject
  def issue: DecisionIssue[P, R] => JsonObject
}

import one.xingyi.cddutilities.AnyLanguage._
class SimpleDecisionTreeRendering[P, R](implicit shortPrintP: ShortPrint[P], shortPrintR: ShortPrint[R]) extends DecisionTreeRendering[P, R] {
  implicit class JsonObjectOps(j: JsonObject) {
    def addOpt(nameAndValues: (String, Option[String])*) = JsonObject(nameAndValues.collect { case (name, Some(value)) => Seq(name -> (value: JsonValue)) }.flatten: _*)
    def data[T](t: T)(implicit getData: HasEngineComponentData[T]): JsonObject = using(getData(t)) { data => j |+| ("defined" -> data.definedInSourceCodeAt.toString) addOpt("title" -> data.title, "comment" -> data.comment) }
    def defined[T](t: T)(implicit isDefinedInSourceCodeAt: IsDefinedInSourceCodeAt[T]) = j |+| ("defined" -> isDefinedInSourceCodeAt(t).toString)
    def addOptList(nameAndList: (String, List[JsonObject])) = if (nameAndList._2.size > 0) j |+| (nameAndList._1 -> JsonList(nameAndList._2), ("has" + nameAndList._1) -> true) else j
  }
  override def engine = ???
  override def tree = tree => JsonObject("tree" -> node(tree.root)) addOptList ("issues" -> tree.issues.map(issue))
  override def useCase = u => JsonObject("useCase" -> JsonObject().data(u))
  override def decisionNode: DecisionNode[P, R] => JsonObject = d => JsonObject("decisionNode" -> JsonObject("condition" -> toJsonString(d.logic.ifString))).defined(d)
  override def conclusionNode: ConclusionNode[P, R] => JsonObject = c => JsonObject("conclusionNode" -> JsonObject("scenarios" -> JsonList(c.scenarios.map(scenario)))).defined(c)
  override def scenario: Scenario[P, R] => JsonObject = s => JsonObject("situation" -> toJsonString(shortPrintP(s.situation))).data(s)
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
