package one.xingyi.cddunit

import one.xingyi.cddengine.Engine
import one.xingyi.cddutilities.AnyLanguage._
import one.xingyi.cddutilities._
import org.junit.runner.notification.{Failure, RunNotifier}
import org.junit.runner.{Description, Result, Runner}

import scala.collection.concurrent.TrieMap
import scala.util.Try
class JunitTestFramework extends TestFramework[Description] with IdMaker {

  def apply(t: CddTest): Description = t match {
    case s: ScenarioTest => Description.createSuiteDescription(s.name, getNextId)
    case s: NestedTest => Description.createSuiteDescription(s.name, getNextId) sideeffect (desc => s.tests.foreach(t => desc.addChild(apply(t))))
  }
}

object NewCddRunner {
  protected def runNestedTest(d: Description, n: NestedTest)(implicit trieMap: TrieMap[Description, CddTest], notifier: RunNotifier) = {
    notifier.fireTestStarted(d)
    d.getChildren.forEach(runDescription)
    notifier.fireTestRunFinished(new Result)
  }

  protected def runScenarioTest(d: Description, s: ScenarioTest)(implicit notifier: RunNotifier) = {
    notifier.fireTestStarted(d)
    Try(s.block()) sideeffectTry {_ fold(e => notifier.fireTestFailure(new Failure(d, e)), _ => notifier.fireTestFinished(d))}
    notifier.fireTestRunFinished(new Result)

  }

  protected def runDescription(d: Description)(implicit trieMap: TrieMap[Description, CddTest], notifier: RunNotifier): Unit =
    trieMap(d) match {
      case n: NestedTest => runNestedTest(d, n)
      case s: ScenarioTest => runScenarioTest(d, s)
    }

  protected def failedTocreate(d: Description)(t: Throwable)(implicit notifier: RunNotifier) = {
    notifier.fireTestStarted(d)
    notifier.fireTestFailure(new Failure(d, t))
  }

}

abstract class NewCddRunner extends Runner with IdMaker {

  protected val framework = new JunitTestFramework

  protected def engines: Try[List[Engine[_, _]]]
  protected val tryTests = engines.map(engineList => NestedTest(getClass.getSimpleName, engineList.map(_.tools.test(s"a engine $getNextId"))))
  protected implicit val trieMap = TrieMap[Description, CddTest]() sideeffect (map => tryTests.foreach(_.fold(map, framework)))
  protected val tryDescription: Try[Description] = tryTests.map(framework)
  override val getDescription: Description = tryDescription.fold(e => Description.createTestDescription(getClass, s"Error$e"), d => d)

  import NewCddRunner._
  override def run(notifier: RunNotifier): Unit = {
    implicit val n = notifier

    notifier.addListener((new Result).createListener)
    notifier.fireTestRunStarted(getDescription)
    tryDescription.fold[Unit](failedTocreate(getDescription), runDescription)
    notifier.fireTestRunFinished(new Result)
  }


}
