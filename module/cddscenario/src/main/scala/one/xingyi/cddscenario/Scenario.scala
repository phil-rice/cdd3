package one.xingyi.cddscenario

import one.xingyi.cddutilities.{IsDefinedInSourceCodeAt, ShortPrint}

import scala.language.higherKinds

trait HasScenarios[T[_, _]] {
  def allScenarios[P, R](t: T[P, R]): List[Scenario[P, R]]
}

object Scenario {
  implicit def scenarioIsDefined[P, R]: IsDefinedInSourceCodeAt[Scenario[P, R]] = _.data.definedInSourceCodeAt
  implicit def scenarioData[P, R]: HasEngineComponentData[Scenario[P, R]] = s => s.data
  implicit def shortPrintFor[P: ShortPrint, R: ShortPrint]: ShortPrint[Scenario[P, R]] = s => s"${ShortPrint(s.situation)} => ${s.result.fold("")(ShortPrint.apply)}"
}
case class Scenario[P, R](situation: P, result: Option[R], logic: SingleScenarioLogic[P, R], assertions: List[ScenarioAssertion[P, R]], data: EngineComponentData) {
  def acceptResult(p: P, r: R) = result.fold(true)(_ == r) && assertions.forall(_.isTrue(p, r))
}


