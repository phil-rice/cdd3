package one.xingyi.cddscenario

import one.xingyi.cddutilities.CddSpec

import scala.language.higherKinds


class ScenarioBuilderSpec extends CddSpec with EngineBuilderLanguage1 {

  behavior of "Scenario Builder"


  it should "have a is defined at" in {
    val (x, scenarios) = new RememberingScenarioAggregator2[Int, String].withAggreator { implicit a =>
      val x = scenario(2) produces "2"
      x.data.definedInSourceCodeAt.toString shouldBe "(ScenarioBuilderSpec.scala:15)"
      x
    }
    scenarios shouldBe List(x.scenario)
  }


  it should "allow scenarios to be created" in {
    val (result, scenarios) = new RememberingScenarioAggregator2[Int, String].withAggreator { implicit a =>
      val x1 = scenario(1) produces "1"
      val x2 = scenario(2) produces "2"
      val y = scenario(2) produces "2" when (_ < 10) title "this is case 2" comment "not very interesting"
      val z = scenario(3) produces "3" because { case x if x < 10 => x.toString }
      List(x1, x2, y, z)
    }
    scenarios shouldBe result.map(_.scenario)
    val List(x1, x2, y, z) = scenarios

    z.logic.fn.isDefinedAt(-5) shouldBe true
    z.logic.fn.isDefinedAt(9) shouldBe true
    z.logic.fn.isDefinedAt(10) shouldBe false
    z.logic.fn.isDefinedAt(11) shouldBe false
    z.logic.fn(3) shouldBe "3"

    scenarios.map(_.data.definedInSourceCodeAt.toString) shouldBe List(
      "(ScenarioBuilderSpec.scala:25)",
      "(ScenarioBuilderSpec.scala:26)",
      "(ScenarioBuilderSpec.scala:27)",
      "(ScenarioBuilderSpec.scala:28)"
    )
  }


}
