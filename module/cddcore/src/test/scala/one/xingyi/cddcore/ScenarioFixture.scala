package one.xingyi.cddcore

import one.xingyi.cddcore.NullScenarioAggregator._
import one.xingyi.cddcore.UntypedScenarioBuilder._
import one.xingyi.cddutilities.CddSpec
import org.scalatest.Matchers

import scala.language.postfixOps

trait ScenarioFixture extends Matchers {
  val snormal = scenario("woman with passport") produces "accept" scenario
  val snormal2 = scenario("man with passport") produces "accept" scenario
  val sNoPassport = scenario("woman") produces "reject" when (!_.contains("passport")) scenario
  val sgun = scenario("man with gun and passport") produces "arrest" because { case x if x contains "gun" => "arrest" } scenario
  val sgunNoPassport = scenario("man with gun ") produces "arrest" scenario
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


trait DecisionTreeFixture extends ScenarioFixture {
  def cn[P, R](s: Scenario[P, R]) = ConclusionNode(List(s), s.logic)
  def cn[P, R](s: List[Scenario[P, R]]) = ConclusionNode(s, s.head.logic)
  def dn[P, R](scond: Scenario[P, R], selse: Scenario[P, R]) = DecisionNode(scond.logic, cn(selse), cn(scond))
  def dt[P, R](scond: List[Scenario[P, R]], selse: List[Scenario[P, R]]) = DecisionNode(scond.head.logic, cn(selse), cn(scond))

  val concEmpty = ConclusionNode[String, String](List(), ScenarioLogic.empty)
  val concNormal = ConclusionNode(List(snormal), snormal.logic)
  val concNormal1And2 = ConclusionNode(List(snormal, snormal2), snormal.logic)
  val concNoPassport = ConclusionNode(List(sNoPassport), sNoPassport.logic)
  val concGunNoPassport = ConclusionNode(List(sgunNoPassport), sgunNoPassport.logic)
  val concGun = ConclusionNode(List(sgun), sgun.logic)
  val concGunGunNoPassport = ConclusionNode(List(sgun, sgunNoPassport), sgun.logic)
  val dNormalPassport = DecisionNode(sNoPassport.logic, concNormal, concNoPassport)
  val dNormalGun = DecisionNode(sgun.logic, concNormal, concGun)
  val dGunNoPassword = DecisionNode(sNoPassport.logic, concGun, concNoPassport)

  val conca = ConclusionNode(List(sa), sa.logic)
  val concawa = ConclusionNode(List(sawa), sawa.logic)
  val concb = ConclusionNode(List(sb), sb.logic)
  val concbwb = ConclusionNode(List(sbwb), sbwb.logic)
}

class ScenarioFixtureSetupSpec extends CddSpec with ScenarioFixture {

  behavior of "ScenarioFixture partial functions"

  it should "for snormal turns any string into 'accept'" in {
    val fn = snormal.logic
    fn("something") shouldBe "accept"
    fn("anything") shouldBe "accept"
  }

  it should "for sNoPassport is only defined when there is no passport, and when defined is 'reject'" in {
    val fn = sNoPassport.logic
    fn.isDefinedAt("person") shouldBe true
    fn.isDefinedAt("person has passport") shouldBe false
    fn("person") shouldBe "reject"
  }
  it should "for sgun is only defined when there is no gun, and when defined is 'arrest'" in {
    val fn = sgun.logic
    fn.isDefinedAt("person") shouldBe false
    fn.isDefinedAt("person has gun") shouldBe true
    fn("person has gun") shouldBe "arrest"
  }

  it should "have a hasCondition method" in {
    sa.logic.hasCondition shouldBe false
    sawa.logic.hasCondition shouldBe true
  }
}

