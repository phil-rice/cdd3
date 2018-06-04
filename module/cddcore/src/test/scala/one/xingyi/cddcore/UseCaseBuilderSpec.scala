package one.xingyi.cddcore
import one.xingyi.cddutilities.CddSpec

import scala.language.reflectiveCalls
class UseCaseBuilderSpec extends CddSpec {

  behavior of "Use case builder"

  it should "allow scenarios to be 'added' to it using the apply method" in {
    var list = List[Scenario[Int, String]]()
    val uc = new UseCase1[Int, String]("some usecase") {
      val s1 = scenario(1) produces "1" because { case x => x.toString }
      val s2 = scenario(2) produces "2"
      list = List(s1.scenario, s2.scenario)
    }
    uc.s1.data.isDefinedAt.toString shouldBe "(UseCaseBuilderSpec.scala:12)"
    uc.s2.data.isDefinedAt.toString shouldBe "(UseCaseBuilderSpec.scala:13)"
    uc.allScenarios shouldBe List(uc.s1.scenario, uc.s2.scenario)
  }
}
