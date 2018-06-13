/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddunit

import java.awt.image.renderable.RenderContext

import one.xingyi.cddengine.{Engine, UseCase}
import one.xingyi.cddscenario.Scenario
import one.xingyi.cddutilities.{IdMaker, Strings}
import org.junit.runner._
import org.junit.runner.notification.{Failure, RunNotifier}

import scala.util.{Success, Try}
trait HasEngines {
  def engines: List[Engine[_, _]]
}

trait MakeDescription[T] extends (T => Description)
object MakeDescription extends IdMaker {

  implicit def makeDescriptionForScenario[P, R]: MakeDescription[Scenario[P, R]] = ???
  implicit def makeDescriptionForUsecase[P, R]: MakeDescription[UseCase[P, R]] = uc => Description.createSuiteDescription(Strings.cleanStringForJunitName(uc.title.getOrElse("Undefined")), getNextId)
  implicit def makeDescriptionForEngine[P, R]: MakeDescription[UseCase[P, R]] = e => Description.createSuiteDescription(Strings.cleanStringForJunitName(uc.title.getOrElse("Unnamed")), getNextId)
}


object AbstractCddRunner {
  def makeDescription[T](t: T)(implicit hasCddDescription: HasCddDescription[T]) =

}
trait AbstractCddRunner extends Runner {

  protected def clazz: Class[_]

  protected def engineData: Try[EngineData]


  class EngineData(val instance: HasEngines) {
    val engines = instance.engines
  }

  var buildExceptionOption: Option[Throwable] = None
  lazy val getDescription: Description = {
    val description = Description.createSuiteDescription(clazz)
    engineData match {
      case Success(engineData) =>
        import engineData._
        engines.foreach(e => description.getChildren.add(makeDescription(e)))
      case util.Failure(e) => buildExceptionOption = Some(e)
    }
    description
  }


  protected def makeDescription(ec: EngineComponent[_, _])(implicit renderContext: RenderContext): Description = {

    val id = renderContext.idPath(ec)
    def create(uc: UseCase[_, _]) = uc.components.reverse.foldLeft(Description.createSuiteDescription(Strings.cleanStringForJunitName(uc.title), id)) {
      (d, child) =>
        d.getChildren.add(makeDescription(child));
        d
    }
    val result = ec match {
      case s: Scenario[_, _] => Description.createSuiteDescription(Strings.cleanStringForJunitName(renderContext.displayProcessor.summary(s)), id)
      case e: Engine[_, _] => create(e.asUseCase)
      case uc: UseCase[_, _] => create(uc)
    }
    ecToDescription = ecToDescription + (ec -> result)
    result
  }


  def run(notifier: RunNotifier): Unit = {

    def runTest(d: Description)(block: => Boolean): Boolean = {
      //      println(s"runTest: $d")
      notifier.fireTestStarted(d)
      try {
        val result = block
        if (result) notifier.fireTestFinished(d)
        else
          notifier.fireTestFailure(new Failure(d, new RuntimeException))
        result
      } catch {
        case e: Exception => notifier.fireTestFailure(new Failure(d, e)); false
      }
    }
    buildExceptionOption match {
      case Some(e) =>
        val result = new Result
        notifier.addListener(result.createListener)
        notifier.fireTestRunStarted(getDescription)
        notifier.fireTestStarted(getDescription)
        notifier.fireTestFailure(new Failure(getDescription, e))
        notifier.fireTestRunFinished(result)
      case None =>
        engineData match {
          case Success(ed) =>
            //        println("Starting the run")
            val result = new Result
            notifier.addListener(result.createListener)
            notifier.fireTestRunStarted(getDescription)
            ed.engines.foreach { engine =>
              engine.decisionTree
              //          println(s"... error scenarios\n.....${engine.errors.mkString("\n.....")}")
              def run[P, R](engineD: Engine[_, _], ecd: EngineComponent[_, _]): Boolean = {
                val engine = engineD.asInstanceOf[Engine[P, R]]
                val ec = ecd.asInstanceOf[EngineComponent[P, R]]
                val d = ecToDescription(ec)
                ec match {
                  case s: Scenario[P, R] if engine.errors.contains(s) => notifier.fireTestStarted(d); notifier.fireTestFailure(new Failure(d, CddRunner.modifyException(engine.errors(s), s.definedInSourceCodeAt))); true
                  case s: Scenario[P, R] => runTest(d)(s.calcuateAssertionFor(engine, s.situation))
                  case uc: UseCase[P, R] => runTest(d)(uc.components.reverse.foldLeft(true)((acc, c) => run(engine, c) && acc))
                  case e: Engine[P, R] => {
                    val result = runTest(d)(e.asUseCase.components.reverse.foldLeft(true)((acc, c) => run(engine, c) && acc))
                    //                    CddContinuousIntegrationTest.makeReports(clazz.getName, "..", engine)
                    result
                  }
                }
              }
              //          println(s"Running engine ${engine.title}")
              run(engine, engine)
            }
            notifier.fireTestRunFinished(result)

          case util.Failure(e) =>
            notifier.fireTestStarted(getDescription)
            notifier.fireTestFailure(new Failure(getDescription, e))
        }
    }
  }
}

object CddRunner {

  def modifyException[E <: Exception](e: E, definedInSourceCodeAt: DefinedInSourceCodeAt): E = {
    e.getStackTrace
    Reflection(e).modField[Array[StackTraceElement]]("stackTrace") { oldSt =>
      if (oldSt.size == 0 || oldSt(0) == definedInSourceCodeAt.st)
        oldSt
      else
        Array(definedInSourceCodeAt.st) ++ oldSt
    }
    e
  }
}

class CddRunner(val clazz: Class[_]) extends AbstractCddRunner {

  lazy val engineData: Try[EngineData] = Try {
    val instance = Reflection.instantiate(clazz).asInstanceOf[HasEngines]
    CddContinuousIntegrationTest.addTest(instance)
    new EngineData(instance)
  }
}


object CddContinuousIntegrationTest {
  private var testList = List[HasEngines]()

  def tests: List[HasEngines] = testList

  def addTest(test: HasEngines) = testList = testList :+ test

  //
  //  def makeReports[P, R](urlOffset: String, referenceBase: String, engine: Engine[P, R])(implicit renderConfiguration: RenderConfiguration) = try {
  //    //    println(s"MakingAReport for $urlOffset and engine ${engine.title}")
  //
  //    renderConfiguration.urlManipulations.populateInitialFiles(renderConfiguration.referenceFilesUrlBase)
  //    Renderer.makeReportFilesFor(urlOffset, referenceBase, engine)
  //
  //  } catch {
  //    case e: Exception => println(e); e.printStackTrace()
  //  }

}

trait CddContinuousIntegrationTest extends HasEngines


