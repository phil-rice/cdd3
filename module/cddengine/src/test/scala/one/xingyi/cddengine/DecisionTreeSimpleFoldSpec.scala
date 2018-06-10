package one.xingyi.cddengine
import one.xingyi.cddscenario.{CompositeScenarioLogic, Scenario}
import one.xingyi.cddutilities.{CddSpec, DefinedInSourceCodeAtLanguage}

class DecisionTreeSimpleFoldSpec extends CddSpec with DecisionTreeFixture with DefinedInSourceCodeAtLanguage{

  val x = implicitly[DecisionTreeFolder[String, String]]

  def folder(s: List[Scenario[String, String]])(implicit f: DecisionTreeFolder[String, String]) = {
    val result = s.foldLeft(DecisionTree.empty[String, String])(f)
    result.issues shouldBe List()
    result.root
  }

  def folderHasIssues(s: List[Scenario[String, String]])(implicit f: DecisionTreeFolder[String, String]) =
    s.foldLeft(DecisionTree.empty[String, String])(f)

  behavior of "Decision tree folding"

  it should "fold an empty list of scenarios and just have the default" ignore {
    val cE = folder(List()).asInstanceOf[ConclusionNode[String, String]]
    cE.logic.definedInSourceCodeAt.toString shouldBe "(DecisionTreeNode.scala:59)"
    cE.scenarios shouldBe List()
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
    folder(List(sa, sbwb)) shouldBe d(sbwb, sa)
    folder(List(sa, sa2, sax, sbwb)) shouldBe d(sbwb.logic, c(sa, sa2, sax), c(sbwb))
    folder(List(sa, sa2, sax, sbwb, sb)) shouldBe d(sbwb.logic, c(sa, sa2, sax), c(sbwb, sb))
  }

  it should "change the logic to use the new scenario if existing scenario hasn't a reason" in {
    folder(List(sa, sawa)) shouldBe ConclusionNode(List(sa, sawa), sawa.logic)
    folder(List(sa, saba)) shouldBe ConclusionNode(List(sa, saba), saba.logic)

  }
  it should "put a scenario without when on the left when a different scenario with when is added" in {
    folder(List(sbwb, sa)) shouldBe d(sbwb, sa)
    folder(List(sbw, sbwb, sa)) shouldBe DecisionNode(sbwb.logic, c(sa), ConclusionNode(List(sbw, sbwb), sbwb.logic))
    folder(List(sb, sbwb, sa)) shouldBe DecisionNode(sbwb.logic, c(sa), ConclusionNode(List(sb, sbwb), sbwb.logic))
  }

  it should "make a composite reason node if new reason given" in {
    folder(List(sawa, saxwx)) shouldBe ConclusionNode(List(sawa, saxwx), CompositeScenarioLogic(Seq(sawa.logic, saxwx.logic)))
    folder(List(sawa, saxww, saxwx)) shouldBe ConclusionNode(List(sawa, saxww, saxwx), CompositeScenarioLogic(Seq(sawa.logic, saxww.logic, saxwx.logic)))
    folder(List(sawa, saxww, saxwx, sa)) shouldBe ConclusionNode(List(sawa, saxww, saxwx, sa), CompositeScenarioLogic(Seq(sawa.logic, saxww.logic, saxwx.logic)))
    folder(List(sa, sawa, saxww, saxwx)) shouldBe ConclusionNode(List(sa, sawa, saxww, saxwx), CompositeScenarioLogic(Seq(sawa.logic, saxww.logic, saxwx.logic)))
  }

  it should "make a decision node with scenario as condition if conclusion and situation come to different results, and conclusion has no condition" in {
    folder(List(sa, sbwb)) shouldBe d(sbwb, sa)
  }
  it should "make a decision node with scenario as condition if conclusion and situation come to different results, and conclusion has no condition, and keep supporting scenarios " in {
    folder(List(sa, sax, sa2, sbwb)) shouldBe d(sbwb.logic, c(sa, sax, sa2), c(sbwb))
  }

  it should "not accept a scenario if it cannot be added because it comes to a different conclusion without a reason in conclusion or scenario" in {
    val DecisionTree(_, issues) = folderHasIssues(List(sabBecomesA, sbbb))
    issues shouldBe List(CannotAddScenarioBecauseClashes(sbbb, List(sabBecomesA)))
  }

}
