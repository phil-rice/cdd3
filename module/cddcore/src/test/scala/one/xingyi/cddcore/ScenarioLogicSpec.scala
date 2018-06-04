package one.xingyi.cddcore
import one.xingyi.cddutilities.CddSpec

class ScenarioLogicSpec extends CddSpec {

  behavior of "ScenarioLogic"

  it should "have an empty method" in {

    val empty = ScenarioLogic.empty[Int, Int]
    empty.code shouldBe None
    empty.definedAt shouldBe None
    empty.result shouldBe None
    empty.definedInSourceCodeAt.toString shouldBe "(Scenario.scala:11)"
  }

}
