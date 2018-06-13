/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddunit

import one.xingyi.cddutilities.Reflection
import org.cddcore.engine.Engine
import org.cddcore.enginecomponents.{EngineComponent, UseCase}
import org.cddcore.rendering.RenderContext
import org.cddcore.utilities._
import org.junit.runner.Description

import scala.collection.JavaConversions._

object DescriptionData {
  def apply(description: Description): DescriptionData = {
    DescriptionData(description.getTestClass, description.getDisplayName,
      Reflection(description).getFieldValue[java.io.Serializable]("fUniqueId").getOrElse(throw new IllegalStateException("Somehow the description didn't have a fUniqueId")).toString, description.getChildren.toList.map(DescriptionData(_)))
  }
}

case class DescriptionData(clazz: Class[_], displayName: String, uniqueId: String, children: List[DescriptionData] = List()) {
  lazy val allChildrenIncludingMe: List[DescriptionData] = this :: children.flatMap(_.allChildrenIncludingMe)
}

object DescriptionDsl {


  implicit object DescriptionHierarchy extends Hierarchy[DescriptionData, DescriptionData] {

    def badChild(topParent: DescriptionData, child: DescriptionData, exception: Exception): DescriptionData = throw exception

    def lastAddedChild(h: DescriptionData): Option[DescriptionData] = h.children.lastOption

    def withNewChild(h: DescriptionData, child: DescriptionData): DescriptionData = h.copy(children = h.children :+ child)

    def childToHolder(child: DescriptionData): DescriptionData = child

    def modChild(h: DescriptionData, fn: (DescriptionData) => DescriptionData): DescriptionData = {
      val child = h.children.last
      val frontOfList = h.children.dropRight(1)
      h.copy(children = frontOfList :+ fn(child))
    }
  }

}


class DescriptionDsl[P, R](clazz: Class[_], val engine: Engine[P, R])(implicit val renderContext: RenderContext) extends MutableHierarchyBuilderWithChildLifeCycle[DescriptionData, DescriptionData] {
  protected implicit def hierarchy = DescriptionDsl.DescriptionHierarchy

  override val postSealMessage: String = "No messing after sealing"

  def description = hierarchyBuilder.holder

  var ecToDescription = Map[EngineComponent[_, _], DescriptionData]()
  val scenarios = engine.allScenarios.toList

  def findScenarioWithSituation(p: P) = scenarios.filter(_.situation == p) match {
    case s :: Nil => s
    case s :: _ => throw new IllegalStateException(s"somehow had multiple scenarios with situation [${p}]")
    case Nil => throw new IllegalArgumentException(s"Cannot find scenario with situation [${p}] Valid situations are ${scenarios.map(_.situation).toList}")
  }

  def findUsecaseWithName(name: String): UseCase[P, R] = findUsecasesWithName(name) match {
    case h :: Nil => h
    case Nil => throw new IllegalArgumentException(s"No use case with name = [${name}] found")
    case l => throw new IllegalStateException(s"Multiple use cases (${l.size}) with name = [${name}] found")
  }

  def findUsecasesWithName(name: String, useCase: UseCase[P, R] = engine.asUseCase): List[UseCase[P, R]] = {
    val rawList: List[UseCase[P, R]] = if (useCase.title == name) List(useCase) else Nil
    rawList ::: useCase.components.flatMap(_ match {
      case uc: UseCase[P, R] => findUsecasesWithName(name, uc)
      case _ => Nil
    })
  }


  protected def makeRootHolder: DescriptionData = DescriptionData(clazz, clazz.getName, clazz.getName)

  protected def engine(engineName: String)(block: => Unit): Unit =
    if (engineName != engine.title)
      throw new IllegalArgumentException(s"Expected: ${engine.title}")
    else
      addParentChildrenDefinedInBlock(DescriptionData(null, engineName, renderContext.pathMap(engine)))(block)

  protected def suite(useCaseName: String)(block: => Unit) = {
    val uc = findUsecaseWithName(useCaseName)
    val id: String = renderContext.pathMap(uc)
    addParentChildrenDefinedInBlock(DescriptionData(null, useCaseName, id))(block)
  }

  protected def test(situation: P) = {
    val scenario = findScenarioWithSituation(situation)
    val id = renderContext.idPath(scenario)
    val summary = Strings.cleanStringForJunitName(renderContext.displayProcessor.summary(scenario))
    val data = DescriptionData(null, summary, id)
    childLifeCycle.created(data)
    ecToDescription = ecToDescription + (scenario -> data)
  }
}

trait DescriptionDslTestFramework {
  val engine = ExampleJUnit.engine1

  implicit val renderContext = {
    import org.cddcore.rendering.Renderer._
    engine.renderContext
  }

  import renderContext._

  val clazz = classOf[ExampleJUnit]
  val dsl = new DescriptionDsl[Int, String](clazz, engine) {
    engine("An engine") {
      suite("a use case") {
        test(1)
        test(2)
      }
      suite("another use case") {
        test(3)
        test(4)
        test(5)
      }
    }
    seal
  }
  val engineId = pathMap(engine)
  val ucs@List(uc1, uc2) = engine.asUseCase.components.reverse
  val ss@List(s1, s2, s3, s4, s5) = dsl.scenarios
  val ds@List(d1, d2, d3, d4, d5) = ss.map(s => Strings.cleanStringForJunitName(displayProcessor.summary(s)))
  val ids@List(ucid1, ucid2, id1, id2, id3, id4, id5) = (ucs ::: ss).map(pathMap(_))

}

class DescriptionDslSpec extends CddSpec with DescriptionDslTestFramework {

  "The description dsl" should "allow the easy construction of description data matching an engine" in {
    dsl.description shouldBe
      DescriptionData(classOf[ExampleJUnit], clazz.getName, clazz.getName, List(
        DescriptionData(null, "An engine", engineId, List(
          DescriptionData(null, "a use case", ucid1, List(
            DescriptionData(null, d1, id1),
            DescriptionData(null, d2, id2)
          )),
          DescriptionData(null, "another use case", ucid2, List(
            DescriptionData(null, d3, id3),
            DescriptionData(null, d4, id4),
            DescriptionData(null, d5, id5)
          )))
        )))
  }

  it should "match the 'getDescription' from CddRunner in " in {
    val actualDescription = new CddRunner(clazz).getDescription
    DescriptionData(actualDescription) shouldBe dsl.description
  }

}
