package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario
import one.xingyi.cddutilities._


trait EngineTester[P, R] extends (String => Engine[P, R] => NestedTest)

case class ScenarioFailedException(s: Scenario[_, _], result: Any) extends RuntimeException

class SimpleTester[P, R](implicit shortPrintp: ShortPrint[P], shortPrintR: ShortPrint[R]) extends EngineTester[P, R] {
  def validate(engine: Engine[P, R])(s: Scenario[P, R]): EngineTest = {
    val name = shortPrintp(s.situation) + "=>" + s.logic.result.fold("undefined")(shortPrintR)
    ScenarioTest(name, () => if (!s.acceptResult(s.situation, engine(s.situation))) throw new ScenarioFailedException(s, engine(s.situation)))
  }

  override def apply(name: String) = { engine: Engine[P, R] =>
    NestedTest(name, engine.tools.useCases.map(uc => NestedTest(uc.title.getOrElse("unnamed"), uc.allScenarios.map(validate(engine)))))
  }

}