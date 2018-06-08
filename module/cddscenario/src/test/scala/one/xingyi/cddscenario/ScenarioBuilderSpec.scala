package one.xingyi.cddscenario

import one.xingyi.cddutilities.CddSpec

import scala.language.higherKinds


class ScenarioBuilderSpec extends CddSpec {
  import one.xingyi.cddscenario.UntypedScenarioBuilder._
  behavior of "Scenario Builder"


  it should "have a is defined at" in {
    val (x, scenarios) = new RememberingScenarioAggregator[Int, String].withAggreator { implicit a =>
    val x = scenario(2) produces "2"
    x.data.isDefinedAt.toString shouldBe "(ScenarioBuilderSpec.scala:15)"
    x
    }
    scenarios shouldBe List(x.scenario)
  }


  it should "allow scenarios to be created" in {
    val (result, scenarios) = new RememberingScenarioAggregator[Int, String].withAggreator { implicit a =>
    val x1 = scenario(1) produces "1"
    val x2 = scenario(2) produces "2"
    val y = scenario(2) produces "2" when (_ < 10) title "this is case 2" comment "not very interesting"
    val z = scenario(3) produces "3" because { case x if x < 10 => x.toString }
    List(x1, x2, y, z)
    }
    scenarios shouldBe result.map(_.scenario)
    val List(x1, x2, y, a) = scenarios

    scenarios.map(_.data.definedInSourceCodeAt.toString) shouldBe List(
      "(ScenarioBuilderSpec.scala:25)",
      "(ScenarioBuilderSpec.scala:26)",
      "(ScenarioBuilderSpec.scala:27)",
      "(ScenarioBuilderSpec.scala:28)"
    )
  }


}
