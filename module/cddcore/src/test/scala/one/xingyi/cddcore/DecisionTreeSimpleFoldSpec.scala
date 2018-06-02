package one.xingyi.cddcore
import one.xingyi.cddutilities.CddSpec

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

}
