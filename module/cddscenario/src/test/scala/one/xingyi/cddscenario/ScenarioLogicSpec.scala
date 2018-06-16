package one.xingyi.cddscenario

import one.xingyi.cddutilities.{CddSpec, DefinedInSourceCodeAt}

class ScenarioLogicSpec extends CddSpec {

  behavior of "ScenarioLogic"

  it should "have an empty method" in {

    val empty:NoScenarioLogic[Int,Int] = ScenarioLogic.empty[Int, Int]
    empty.ifString shouldBe "<empty>"
    empty.definedInSourceCodeAt.toString shouldBe "(ScenarioLogicSpec.scala:11)"
  }


}
