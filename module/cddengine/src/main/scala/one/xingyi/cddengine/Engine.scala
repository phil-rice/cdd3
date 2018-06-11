package one.xingyi.cddengine
import one.xingyi.cddscenario.{HasScenarios, Scenario, ScenarioLogic}
import one.xingyi.cddutilities.json.{JsonMaps, JsonObject, JsonWriter, TemplateEngine}

import scala.language.higherKinds

object JsonDataForTree {
  def make[J: JsonWriter, P, R](data: WithScenarioData[P, R])(jsonObject: JsonObject): JsonDataForTree[J, P, R] = JsonDataForTree[J, P, R](jsonObject, Some(data))
}
case class JsonDataForTree[J: JsonWriter, P, R](jsonObject: JsonObject, data: Option[WithScenarioData[P, R]]) extends JsonMaps[J](jsonObject)

trait Engine[P, R] extends PartialFunction[P, R] {
  def tools: EngineTools[P, R]

}

trait EngineTools[P, R] {
  def useCases: List[UseCase[P, R]]
  def scenarios: List[Scenario[P, R]]
  def decisionTree: DecisionTree[P, R]
  def trace[J: JsonWriter](filePattern: String)(implicit template: TemplateEngine[J], printRenderToFile: PrintRenderToFile): Unit
  def printPages[J: JsonWriter](filePattern: String)(implicit template: TemplateEngine[J], printRenderToFile: PrintRenderToFile): Unit
}

class SimpleEngineTools[P, R](engine: Engine1[P, R]) extends EngineTools[P, R] {
  override def decisionTree: DecisionTree[P, R] = engine.decisionTree
  override def scenarios: List[Scenario[P, R]] = engine.scenarios
  override def useCases: List[UseCase[P, R]] = engine.useCases
  protected def printPrinter[J: JsonWriter](implicit template: TemplateEngine[J]) = DecisionTreeRendering.simple[P, R] andThen (x => JsonDataForTree[J, P, R](x, None)) andThen template.apply
  protected def tracePrinter[J: JsonWriter](data: WithScenarioData[P, R])(implicit template: TemplateEngine[J]) = DecisionTreeRendering.withScenario[P, R](data) andThen JsonDataForTree.make[J, P, R](data) andThen template.apply
  def trace[J: JsonWriter](filePattern: String)(implicit template: TemplateEngine[J], printRenderToFile: PrintRenderToFile): Unit = {
    DecisionTreeRendering.trace(tracePrinter[J])(filePattern)(engine)
  }
  override def printPages[J: JsonWriter](filePattern: String)(implicit template: TemplateEngine[J], printRenderToFile: PrintRenderToFile): Unit = {
    DecisionTreeRendering.print.apply[P, R](printPrinter[J], tracePrinter[J])(filePattern, engine)
  }
}
case class Engine1[P, R](decisionTree: DecisionTree[P, R], scenarios: List[Scenario[P, R]], useCases: List[UseCase[P, R]]) extends Engine[P, R] {
  def logicFor(p: P): ScenarioLogic[P, R] = decisionTree.root.findLens(p).get(decisionTree.root).logic
  override def isDefinedAt(p: P): Boolean = logicFor(p).fn.isDefinedAt(p)
  override def apply(p: P): R = logicFor(p).fn apply p //later we can be more efficient. Don't optimise just yet
  override val tools: EngineTools[P, R] = new SimpleEngineTools(this)
}

object Engine {
  def apply[T[_, _], P, R](t: T[P, R])(implicit hasScenarios: HasScenarios[T], hasUseCases: HasUseCases[T], dtFolder: DecisionTreeFolder[P, R]): Engine[P, R] = {
    val scenarios = hasScenarios.allScenarios[P, R](t)
    val decisionTree = scenarios.foldLeft(DecisionTree.empty[P, R])(dtFolder)
    Engine1(decisionTree, scenarios, hasUseCases.useCases[P, R](t))
  }
  //  def apply[P, R](title: String)(block: ScenarioAggregator[P, R] => Unit): Engine[P, R] = Engine1(UseCase(title)(block).allScenarios)

}
