package one.xingyi.cddscenario

import one.xingyi.cddutilities.CddSpec
import org.scalatest.Matchers

import scala.language.postfixOps

trait ScenarioFixture extends Matchers with EngineBuilderLanguage1 {
  private implicit val nullScenarioAggregator = NullScenarioAggregator2.nullAggregator[String, String]
  val snormal = scenario("woman with passport") produces "accept" scenario
  val snormal2 = scenario("man with passport") produces "accept" scenario
  val sNoPassport = scenario("woman") produces "reject" when (!_.contains("passport")) scenario
  val sgun = scenario("man with gun and passport") produces "arrest" because { case x if x contains "gun" => "arrest" } scenario
  val sgunNoPassport = scenario("man with gun") produces "arrest" scenario
  val default = { s: String => fail("default is to fail") }

  val sa = scenario("a") produces "A" scenario
  val sa2 = scenario("aw") produces "A" scenario
  val sax = scenario("ax") produces "A" scenario
  val sabBecomesA = scenario("ab") produces "A" scenario
  val saxww = scenario("aw") produces "A" when (_ contains "w") scenario
  val saxwx = scenario("ax") produces "A" when (_ contains "x") scenario

  val sawa = scenario("a") produces "A" when (_ contains "a") scenario
  val saba = scenario("a") produces "A" because { case x if x == "a" => "A" } scenario
  val sb = scenario("b") produces "B" scenario

  val sbw = scenario("bw") produces "B" scenario
  val sbx = scenario("bx") produces "B" scenario
  val sbbb = scenario("b") produces "B" because { case b if b contains "b" => "B" } scenario
  val sbwb = scenario("b") produces "B" when (_ contains "b") scenario
  val sab = scenario("ab") produces "AB" scenario
  val sabwab = scenario("ab") produces "AB" when (_ contains "b") scenario
}

class ScenarioFixtureSetupSpec extends CddSpec with ScenarioFixture {

  behavior of "ScenarioFixture partial functions"

  it should "for snormal turns any string into 'accept'" in {
    val fn = snormal.logic.fn
    fn("something") shouldBe "accept"
    fn("anything") shouldBe "accept"
  }

  it should "for sNoPassport is only defined when there is no passport, and when defined is 'reject'" in {
    val fn = sNoPassport.logic.fn
    fn.isDefinedAt("person") shouldBe true
    fn.isDefinedAt("person has passport") shouldBe false
    fn("person") shouldBe "reject"
  }
  it should "for sgun is only defined when there is no gun, and when defined is 'arrest'" in {
    val fn = sgun.logic.fn
    fn.isDefinedAt("person") shouldBe false
    fn.isDefinedAt("person has gun") shouldBe true
    fn("person has gun") shouldBe "arrest"
  }

  it should "have a hasCondition method" in {
    sa.logic.hasCondition shouldBe false
    sawa.logic.hasCondition shouldBe true
  }
}

