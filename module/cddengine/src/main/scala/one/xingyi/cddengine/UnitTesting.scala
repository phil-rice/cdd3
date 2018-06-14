package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario
import one.xingyi.cddutilities._

import scala.util.Try

trait CddRunner extends IdMaker {
  protected def engines: Try[List[Engine[_, _]]]
  protected lazy val tryTests = engines.map(engineList => NestedTest(getClass.getSimpleName, engineList.map(_.tools.test(s"a engine $getNextId"))))

}


case class ScenarioFailedException(s: Scenario[_, _], result: Any) extends RuntimeException

class SimpleTestMaker[P, R](name: String, engine: Engine[P, R])(implicit shortPrintp: ShortPrint[P], shortPrintR: ShortPrint[R]) {
  def validate(engine: Engine[P, R])(s: Scenario[P, R]): CddTest = {
    val name = shortPrintp(s.situation) + "=>" + s.logic.result.fold("undefined")(shortPrintR)
    ScenarioTest(name, () => if (!s.acceptResult(s.situation, engine(s.situation))) throw new ScenarioFailedException(s, engine(s.situation)))
  }

  def apply =
    NestedTest(name, engine.tools.useCases.map(uc => NestedTest(uc.title.getOrElse("unnamed"), uc.allScenarios.map(validate(engine)))))

}