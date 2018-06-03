package one.xingyi.cddcore
import one.xingyi.cddutilities.DefinedInSourceCodeAt

case class UseCase[P, R](components: List[EngineComponent[P, R]] = List(), data: EngineComponentData) extends EngineComponent[P, R]

object UseCase {
  def apply[P, R](title: String)(block: ScenarioAggregator[P, R] => Unit): UseCase[P, R] = {
    val (_, scenarios) = new RememberingScenarioAggregator[P, R].withAggreator(block)
    UseCase(scenarios, EngineComponentData(DefinedInSourceCodeAt.definedInSourceCodeAt(), Some(title)))
  }
}