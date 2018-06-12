package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario
import one.xingyi.cddutilities._


trait EngineTester[J, P, R] extends (String => Engine[P, R] => (J, NestedTest))


class SimpleTester[J, P, R](implicit shortPrintp: ShortPrint[P], shortPrintR: ShortPrint[R], saveTest: SaveTest[J]) extends EngineTester[J, P, R] {
  def validate(engine: Engine[P, R])(s: Scenario[P, R]): EngineTest = {
    val name = shortPrintp(s.situation) + " => " + s.logic.result.fold("not defined")(shortPrintR)
    if (s.acceptResult(s.situation, engine(s.situation))) ScenarioTest(name, TestSucceeded) else ScenarioTest(name, TestFailed)
  }

  override def apply(name: String) = { engine: Engine[P, R] =>
    val result = NestedTest(name, engine.tools.useCases.map(uc => NestedTest(uc.title.getOrElse("unnamed"), uc.allScenarios.map(validate(engine)))))
    (saveTest(result), result)
  }
}