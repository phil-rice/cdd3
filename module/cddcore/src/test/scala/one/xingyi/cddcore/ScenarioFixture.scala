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


class ConclusionAndScenarioStrategyFinderSpec extends CddSpec with DecisionTreeFixture {
  val editor = implicitly[ConclusionAndScenarioStrategyFinder[String, String]]

  behavior of "ConclusionNodeEditor"

  def check[P, R](pf: PartialFunction[(ConclusionNode[P, R], Scenario[P, R]), DecisionTreeNode[P, R]])(c: ConclusionNode[P, R], s: Scenario[P, R]) = {
    pf.isDefinedAt(c, s) shouldBe true
    pf(c, s)
  }
  //  it should "allow scenarios to be added to an empty conclusion" in {
  //    check(editor.empty)(concEmpty, snormal) shouldBe concNormal
  //    check(editor.empty)(concEmpty, sgun) shouldBe concGun
  //    editor.empty.isDefinedAt(concNormal, snormal) shouldBe false
  //  }
  //
  //  it should "add a scenario to a compatible conclusion" in {
  //    check(editor.compatible)(concNormal, snormal2) shouldBe concNormal1And2
  //    editor.compatible.isDefinedAt(concGun, snormal) shouldBe false
  //  }
  //
  //  it should "make a decision node with scenario on left, if scenario doesn't match the partial function of the conclusion" in {
  //    implicit val splitter = new ScenarioSplitter(concGun, snormal)
  //    check(editor.scenarioInLeft)(concGun, snormal) shouldBe dNormalGun
  //  }
  //
  //  it should "make a decision node with scenario on right, if conclusion can be differentiated by the scenario and conclusion has no condition" in {
  //    implicit val splitter = new ScenarioSplitter(cn(sa), sbwb)
  //    splitter.canUseScenarioAsCondition shouldBe true
  //    check(editor.useScenarioAsConclusionHasNoCondition)(cn(sa), sbwb) shouldBe dn(sbwb, sa)
  //
  //  }
  //  it should "make a decision node with scenario on right, if conclusion can be differentiated by the scenario2" in {
  //    implicit val splitter = new ScenarioSplitter(concNormal, sNoPassport)
  //    splitter.canUseScenarioAsCondition shouldBe true
  //    check(editor.scenarioInRight)(concNormal, sNoPassport) shouldBe dNormalPassport
  //  }
  //

  //  it should "add a second scenario as a decision node on the left, the second scenario is not matched by the scenario's reason" in {
  //    editor.
  //      folder(List(sgun, snormal)) shouldBe dNormalGun
  //  }


}
class DecisionTreeSimpleFoldSpec extends CddSpec with DecisionTreeFixture {

  val x = implicitly[DecisionTreeFolder[String, String]]

  def folder(s: List[Scenario[String, String]])(implicit f: DecisionTreeFolder[String, String]) = {
    val result = s.foldLeft(DecisionTree.empty[String, String])(f)
    result.issues shouldBe List()
    result.root
  }

  def folderHasIssues(s: List[Scenario[String, String]])(implicit f: DecisionTreeFolder[String, String]) =
    s.foldLeft(DecisionTree.empty[String, String])(f)

  behavior of "Decision tree folding"

  it should "fold an empty list of scenarios and just have the default" in {
    folder(List()) shouldBe concEmpty
  }
  it should "fold a scenario and have a conclusion with just that scenario" in {
    folder(List(snormal)) shouldBe concNormal
    folder(List(sgun)) shouldBe concGun
    folder(List(snormal, snormal2)) shouldBe concNormal1And2
  }

  it should "add supporting scenarios to conclusion nodes" in {
    folder(List(snormal, snormal2)) shouldBe concNormal1And2
    folder(List(sgun, sgunNoPassport)) shouldBe concGunGunNoPassport
  }

  it should "put the scenario on the right when a different scenario without when is added" in {
    folder(List(sa, sbwb)) shouldBe dn(sbwb, sa)
    folder(List(sa, sa2, sax, sbwb)) shouldBe dt(List(sbwb), List(sa, sa2, sax))
    folder(List(sa, sa2, sax, sbwb, sb)) shouldBe dt(List(sbwb, sb), List(sa, sa2, sax))
  }

  it should "change the logic to use the new scenario if existing scenario hasn't a reason" in {
    folder(List(sa, sawa)) shouldBe ConclusionNode(List(sa, sawa), sawa.logic)
    folder(List(sa, saba)) shouldBe ConclusionNode(List(sa, saba), saba.logic)

  }
  it should "put a scenario without when on the left when a different scenario with when is added" in {
    folder(List(sbwb, sa)) shouldBe dn(sbwb, sa)
    folder(List(sbw, sbwb, sa)) shouldBe DecisionNode(sbwb.logic, cn(sa), ConclusionNode(List(sbw, sbwb), sbwb.logic))
    folder(List(sb, sbwb, sa)) shouldBe DecisionNode(sbwb.logic, cn(sa), ConclusionNode(List(sb, sbwb), sbwb.logic))
  }

  it should "make a composite reason node if new reason given" in {
    folder(List(sawa, saxwx)) shouldBe ConclusionNode(List(sawa, saxwx), sawa.logic or saxwx.logic)
    folder(List(sawa, saxww, saxwx)) shouldBe ConclusionNode(List(sawa, saxww, saxwx), CompositeScenarioLogic(Seq(sawa.logic, saxww.logic, saxwx.logic)))
    folder(List(sawa, saxww, saxwx, sa)) shouldBe ConclusionNode(List(sawa, saxww, saxwx, sa), CompositeScenarioLogic(Seq(sawa.logic, saxww.logic, saxwx.logic)))
    folder(List(sa, sawa, saxww, saxwx)) shouldBe ConclusionNode(List(sa, sawa, saxww, saxwx), CompositeScenarioLogic(Seq(sawa.logic, saxww.logic, saxwx.logic)))
  }

  it should "make a decision node with scenario as condition if conclusion and situation come to different results, and conclusion has no condition" in {
    folder(List(sa, sbwb)) shouldBe dn(sbwb, sa)
  }
  it should "make a decision node with scenario as condition if conclusion and situation come to different results, and conclusion has no condition, and keep supporting scenarios " in {
    folder(List(sa, sax, sa2, sbwb)) shouldBe DecisionNode(sbwb.logic, ConclusionNode(List(sa, sax, sa2), sa.logic), cn(sbwb))
  }

  it should "not accept a scenario if it cannot be added because it comes to a different conclusion without a reason in conclusion or scenario" in {
    val DecisionTree(_, issues) = folderHasIssues(List(sabBecomesA, sbbb))
    issues shouldBe List(CannotAddScenarioBecauseClashes(sbbb, List(sabBecomesA)))
  }


  it should "move the scenarios already i"

  //  it should "use " in {
  //    folder(List(sNoPassport, sgun)) shouldBe dGunNoPassword
  //    folder(List(sNoPassport, sgunNoPassport)) shouldBe DecisionNode(sgunNoPassport.logic, concNoPassport, concGunNoPassport)
  //    //    editor.
  //    //      folder(List(sgun, snormal)) shouldBe dNormalGun
  //    //  }
  //  }
}