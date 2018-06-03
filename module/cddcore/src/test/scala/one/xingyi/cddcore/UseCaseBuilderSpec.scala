package one.xingyi.cddcore
import one.xingyi.cddutilities.CddSpec
import UntypedScenarioBuilder._
class UseCaseBuilderSpec extends CddSpec {

  behavior of "Use case builder"

  it should "allow scenarios to be 'added' to it using the apply method" in {
    var list = List[Scenario[Int, String]]()
    val uc = UseCase[Int, String]("some usecase") { implicit a =>
      val s1 = scenario(1) produces "1" because { case x => x.toString }
      val s2 = scenario(2) produces "2"
      list = List(s1.scenario, s2.scenario)
    }
    uc.components shouldBe list
    uc.components.map(_.data.definedInSourceCodeAt.toString) shouldBe List("(UseCaseBuilderSpec.scala:11)","(UseCaseBuilderSpec.scala:12)")
  }
}
