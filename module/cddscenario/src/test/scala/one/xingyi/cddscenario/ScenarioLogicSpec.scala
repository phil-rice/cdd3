package one.xingyi.cddscenario

import one.xingyi.cddutilities.{CddSpec, DefinedInSourceCodeAt}

class ScenarioLogicSpec extends CddSpec {

  behavior of "ScenarioLogic"

  it should "have an empty method" in {

    val empty = ScenarioLogic.empty[Int, Int]
    empty.code shouldBe None
    empty.definedAt shouldBe None
    empty.result shouldBe None
    empty.definedInSourceCodeAt.toString shouldBe "(ScenarioLogicSpec.scala:11)"
  }


}
