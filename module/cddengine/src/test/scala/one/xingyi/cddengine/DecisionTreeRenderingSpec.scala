package one.xingyi.cddengine
import one.xingyi.cddutilities.CddSpec
import one.xingyi.cddutilities.json._

class DecisionTreeRenderingSpec extends CddSpec with DecisionTreeFixture with JsonWriterLangauge {

  behavior of "SimpleDecisionTreeRendering"

  val simple = DecisionTreeRendering.simple[String, String]
  it should "render a scenario" in {
    simple.scenario(sNoPassport) shouldBe JsonObject("situation" -> "woman", "url" -> "scenario_0.html", "defined" -> sNoPassport.data.definedInSourceCodeAt.toString)
  }

  it should "render a conclusion node" in {
    sgun.definedInSourceCodeAt.toString shouldBe "(ScenarioFixture.scala:13)"
    sgunNoPassport.definedInSourceCodeAt.toString shouldBe "(ScenarioFixture.scala:14)"
    simple.node(concGunGunNoPassport) shouldBe JsonObject(
      "conclusionNode" -> JsonObject("scenarios" -> JsonList(List(
        JsonObject("situation" -> "man with gun and passport", "url" -> "scenario_1.html", "defined" -> sgun.definedInSourceCodeAt.toString),
        JsonObject("situation" -> "man with gun", "url" -> "scenario_2.html", "defined" -> sgunNoPassport.definedInSourceCodeAt.toString)))),
      "defined" ->sgun.definedInSourceCodeAt.toString)
  }
  it should "render a decision node" in {
    //    simple.node(dNormalGun) shouldBe JsonObject("decisionNode" -> JsonObject("condition" -> "not yet"), "defined" -> "(ScenarioFixture.scala:14)")
  }

  it should ""
}
