package one.xingyi.cddengine
import one.xingyi.cddutilities.CddSpec
import one.xingyi.cddutilities.json._

class DecisionTreeRenderingSpec extends CddSpec with DecisionTreeFixture with JsonWriterLangauge {

  behavior of "SimpleDecisionTreeRendering"

  val simple = DecisionTreeRendering.simple[String, String]
  it should "render a scenario" in {
    simple.scenario(sNoPassport) shouldBe JsonObject("situation" -> "woman", "defined" -> "(ScenarioFixture.scala:13)")
  }

  it should "render a conclusion node" in {
    simple.node(concGunGunNoPassport) shouldBe JsonObject(
      "conclusionNode" -> JsonObject("scenarios" -> JsonList(List(
        JsonObject("situation" -> "man with gun and passport", "defined" -> "(ScenarioFixture.scala:14)"),
        JsonObject("situation" -> "man with gun", "defined" -> "(ScenarioFixture.scala:15)")))),
      "defined" -> "(ScenarioFixture.scala:14)")
  }
  it should "render a decision node" in {
//    simple.node(dNormalGun) shouldBe JsonObject("decisionNode" -> JsonObject("condition" -> "not yet"), "defined" -> "(ScenarioFixture.scala:14)")
  }

  it should ""
}
