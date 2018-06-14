package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario
import one.xingyi.cddutilities._
import AnyLanguage._
import scala.util.Try

trait CddRunner extends IdMaker {
  protected def engines: Try[List[Engine[_, _]]]
  protected lazy val tryTests = engines.map(engineList => NestedTest(getClass.getSimpleName, engineList.map(_.tools.test(s"a engine $getNextId"))))

}


case class ScenarioFailedException[R](s: Scenario[_, _], result: R)(implicit short: ShortPrint[R]) extends
  RuntimeException(s"Actual result: ${short(result)}")

class SimpleTestMaker[P, R](name: String, engine: Engine[P, R])(implicit shortPrintp: ShortPrint[P], shortPrintR: ShortPrint[R]) {
  def checkResult(s: Scenario[P, R]) = engine(s.situation) sideeffect (result => if (!s.acceptResult(s.situation, result))throw new ScenarioFailedException(s, result))

  def validate(engine: Engine[P, R])(s: Scenario[P, R]): CddTest = {
    val name = shortPrintp(s.situation) + "=>" + s.logic.result.fold("undefined")(shortPrintR)
    ScenarioTest(name, () => checkResult(s))
  }

  def apply =
    NestedTest(name, engine.tools.useCases.map(uc => NestedTest(uc.title.getOrElse("unnamed"), uc.allScenarios.map(validate(engine)))))

}