package one.xingyi.cddcore
import one.xingyi.cddutilities.DefinedInSourceCodeAt

case class UseCase[P, R](components: List[EngineComponent[P, R]] = List(), data: EngineComponentData) extends EngineComponent[P, R] {
  def allScenarios: List[Scenario[P, R]] = components.collect { case s: Scenario[P, R] => List(s); case uc: UseCase[P, R] => uc.allScenarios }.flatten
}

object UseCase {
  def apply[P, R](title: String)(block: ScenarioAggregator[P, R] => Unit): UseCase[P, R] = {
    val (_, scenarios) = new RememberingScenarioAggregator[P, R].withAggreator(block)
    UseCase(scenarios, EngineComponentData(DefinedInSourceCodeAt.definedInSourceCodeAt(), Some(title)))
  }
}