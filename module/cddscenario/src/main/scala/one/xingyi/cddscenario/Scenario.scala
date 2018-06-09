package one.xingyi.cddscenario

import one.xingyi.cddutilities.IsDefinedInSourceCodeAt

import scala.language.higherKinds

trait HasScenarios[T[_, _]] {
  def allScenarios[P, R](t: T[P, R]): List[Scenario[P, R]]
}

object Scenario {
  implicit def scenarioIsDefined[P, R]: IsDefinedInSourceCodeAt[Scenario[P, R]] = _.data.definedInSourceCodeAt
}
case class Scenario[P, R](situation: P, logic: SingleScenarioLogic[P, R], assertions: List[ScenarioAssertion[P, R]], data: EngineComponentData) {
  def acceptResult(p: P, r: R) = logic.result.fold(true)(_ == r) && assertions.forall(_.isTrue(p, r))
}


