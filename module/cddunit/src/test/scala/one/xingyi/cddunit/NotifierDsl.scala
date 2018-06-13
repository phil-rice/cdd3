/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddunit

import org.cddcore.engine.{ConflictingScenariosException, ConflictingScenariosException$}
import org.cddcore.enginecomponents.ReasonInvalidException
import org.cddcore.utilities.{CddSpec, Hierarchy, MutableHierarchyBuilderWithChildLifeCycle, Reflection}
import org.junit.runner.notification.{Failure, RunNotifier}
import org.junit.runner.{Description, Result}

trait JunitNotification {
  override def toString = getClass.getSimpleName.dropRight(1)
}

object TestRunStarted extends JunitNotification

object TestRunFinished extends JunitNotification

object TestStarted extends JunitNotification

object TestFinished extends JunitNotification

case class TestFailed(clazz: Class[_ <: Throwable]) extends JunitNotification {
  override def toString = s"TestFailed(classOf[${clazz.getSimpleName}])"
}

trait NotifierData

case class NotifierEvent(id: String, notification: JunitNotification) extends NotifierData {
  override def toString = s"""NotifierEvent("$id", $notification)"""
}

case class NotifierHolder(events: Vector[NotifierData]) extends NotifierData {
  override def toString = s"NotifierHolder(Vector(\n   ${events.mkString(",\n   ")}))"
}

class NotifierDsl[P, R](descriptionDsl: DescriptionDsl[P, R]) extends MutableHierarchyBuilderWithChildLifeCycle[NotifierHolder, NotifierData] {
  implicit protected def hierarchy: Hierarchy[NotifierHolder, NotifierData] = new Hierarchy[NotifierHolder, NotifierData] {

    def badChild(topParent: NotifierHolder, child: NotifierData, exception: Exception): NotifierHolder = ???

    def lastAddedChild(h: NotifierHolder): Option[NotifierData] = h.events.lastOption

    def withNewChild(h: NotifierHolder, child: NotifierData): NotifierHolder = h.copy(events = h.events :+ child)

    def childToHolder(child: NotifierData): NotifierHolder = ???

    override def modChild(h: NotifierHolder, fn: (NotifierData) => NotifierData): NotifierHolder = {
      val withoutLast = h.events.dropRight(1)
      val newValue = withoutLast :+ fn(h.events.last)
      h.copy(events = newValue)
    }
  }

  override def postSealMessage: String = ???

  protected def makeRootHolder: NotifierHolder = new NotifierHolder(Vector())

  implicit val renderContext = descriptionDsl.renderContext

  def testRun(block: => Unit) = {
    addChild(NotifierEvent(descriptionDsl.description.uniqueId, TestRunStarted))
    block
    addChild(NotifierEvent(descriptionDsl.description.uniqueId, TestRunFinished))
  }

  def engine(block: => Unit) = {
    val id = renderContext.pathMap(descriptionDsl.engine)
    addChild(NotifierEvent(id, TestStarted))
    block
    addChild(NotifierEvent(id, TestFinished))

  }

  def testSuite(useCaseName: String)(block: => Unit): Unit = {
    val id = renderContext.pathMap(descriptionDsl.findUsecaseWithName(useCaseName))
    descriptionDsl.description.allChildrenIncludingMe.find(_.uniqueId == id) match {
      case Some(d) =>
        addChild(NotifierEvent(d.uniqueId, TestStarted))
        block
        addChild(NotifierEvent(d.uniqueId, TestFinished))
      case None => throw new IllegalStateException(s"Description with id $id was not found")
    }
  }

  def scenario(p: P) = {
    val id = renderContext.pathMap(descriptionDsl.findScenarioWithSituation(p))
    addChild(NotifierEvent(id, TestStarted))
    addChild(NotifierEvent(id, TestFinished))
  }

  def scenarioFailure[E <: Throwable](p: P, exceptionClass: Class[E]) = {
    val id = renderContext.pathMap(descriptionDsl.findScenarioWithSituation(p))
    addChild(NotifierEvent(id, TestStarted))
    addChild(NotifierEvent(id, TestFailed(exceptionClass)))
  }
}


class NotifierDslSpec extends CddSpec with DescriptionDslTestFramework {
  val notifierDsl = new NotifierDsl[Int, String](dsl) {
    testRun {
      engine {
        testSuite("a use case") {
          scenario(1)
          scenarioFailure(2, classOf[ReasonInvalidException[_, _]])
        }
        testSuite("another use case") {
          scenario(3)
          scenarioFailure(4, classOf[ConflictingScenariosException[_, _]])
          scenarioFailure(5, classOf[ConflictingScenariosException[_, _]])
        }
      }
    }
  }

  "The NotifierDsl" should "make easy to read lists of notifications" in {
    notifierDsl.hierarchyBuilder.holder shouldBe NotifierHolder(Vector(
      NotifierEvent("org.cddcore.cddunit.ExampleJUnit", TestRunStarted),
      NotifierEvent("An engine/index", TestStarted),
      NotifierEvent("An engine/1", TestStarted),
      NotifierEvent("An engine/1.1", TestStarted),
      NotifierEvent("An engine/1.1", TestFinished),
      NotifierEvent("An engine/1.2", TestStarted),
      NotifierEvent("An engine/1.2", TestFailed(classOf[ReasonInvalidException[_, _]])),
      NotifierEvent("An engine/1", TestFinished),
      NotifierEvent("An engine/2", TestStarted),
      NotifierEvent("An engine/2.1", TestStarted),
      NotifierEvent("An engine/2.1", TestFinished),
      NotifierEvent("An engine/2.2", TestStarted),
      NotifierEvent("An engine/2.2", TestFailed(classOf[ConflictingScenariosException[_, _]])),
      NotifierEvent("An engine/2.3", TestStarted),
      NotifierEvent("An engine/2.3", TestFailed(classOf[ConflictingScenariosException[_, _]])),
      NotifierEvent("An engine/2", TestFinished),
      NotifierEvent("An engine/index", TestFinished),
      NotifierEvent("org.cddcore.cddunit.ExampleJUnit", TestRunFinished)))
  }

  class RememberRunner extends RunNotifier {
    var descriptions = List[Description]()
    var events = List[NotifierEvent]()

    def holder = NotifierHolder(events.toVector.reverse)

    def popDescriptions = {
      val result = descriptions.head
      descriptions = descriptions.tail
      result
    }

    implicit def descriptionToId(description: Description) = Reflection(description).getFieldValue[java.io.Serializable]("fUniqueId").getOrElse(throw new IllegalStateException("Field fUniqueId not found")).toString

    override def fireTestFailure(failure: Failure): Unit = {
      val description = popDescriptions
      events = NotifierEvent(description, TestFailed(failure.getException.getClass)) :: events
      super.fireTestFailure(failure)
    }

    override def fireTestFinished(description: Description): Unit = {
      val description = popDescriptions
      events = NotifierEvent(description, TestFinished) :: events
      super.fireTestFinished(description)
    }

    override def fireTestRunFinished(result: Result): Unit = {
      super.fireTestRunFinished(result)
      val description = popDescriptions
      events = NotifierEvent(description, TestRunFinished) :: events
    }

    override def fireTestRunStarted(description: Description): Unit = {
      descriptions = description :: descriptions
      events = NotifierEvent(description, TestRunStarted) :: events
      super.fireTestRunStarted(description)
    }

    override def fireTestStarted(description: Description): Unit = {
      super.fireTestStarted(description)
      descriptions = description :: descriptions
      events = NotifierEvent(description, TestStarted) :: events
    }
  }

  it should "allow checks against CddRunner notifications" in {
    val runner = new CddRunner(clazz)
    val notifier = new RememberRunner
    runner.run(notifier)
    notifier.holder shouldBe new NotifierHolder(notifierDsl.hierarchyBuilder.holder.events)

  }

}
