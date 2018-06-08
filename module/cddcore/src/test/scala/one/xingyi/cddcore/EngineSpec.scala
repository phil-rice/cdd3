package one.xingyi.cddcore
import one.xingyi.cddscenario.ScenarioLogic.SingleScenarioLogic
import one.xingyi.cddutilities.CddSpec
class EngineSpec extends CddSpec with DecisionTreeFixture {

  behavior of "Engine smoke tests"

  val containsA = { s: String => s contains "a" }
  val containsB = { s: String => s contains "b" }

  it should "not smoke" in {

    val e = Engine(new UseCase1[String, String]("engine") {
      scenario("a") produces "A" when containsA
      scenario("aw") produces "A"
      scenario("b") produces "B"
      scenario("bw") produces "B"
    }).asInstanceOf[Engine1[String, String]]

    val DecisionTree(DecisionNode(dlLogic: SingleScenarioLogic[String, String],
    ConclusionNode(List(sb, sbw), falseLogic: SingleScenarioLogic[String, String]),
    ConclusionNode(List(sa, saw), trueLogic: SingleScenarioLogic[String, String])), List()) = e.dt
    sb.situation shouldBe "b"
    sbw.situation shouldBe "bw"
    sa.situation shouldBe "a"
    saw.situation shouldBe "aw"
    dlLogic.definedAt shouldBe Some(containsA)
    trueLogic shouldBe dlLogic

    e("a") shouldBe "A"
    e("b") shouldBe "B"
    e("c") shouldBe "B"
  }
  it should "not smoke with deeper tests" in {

    val e = Engine(new UseCase1[String, String]("engine") {
      scenario("a") produces "A" when containsA
      scenario("aw") produces "A"
      scenario("c") produces "C"
      scenario("cw") produces "C"
      scenario("b") produces "B" when containsB
      scenario("bw") produces "B"

    }).asInstanceOf[Engine1[String, String]]

    val DecisionTree(
    DecisionNode(dlLogic: SingleScenarioLogic[String, String],
    DecisionNode(ifcLogic: SingleScenarioLogic[String, String],
    ConclusionNode(List(sc, scw), cLogic: SingleScenarioLogic[String, String]),
    ConclusionNode(List(sb, sbw), bLogic: SingleScenarioLogic[String, String])),
    ConclusionNode(List(sa, saw), aLogic: SingleScenarioLogic[String, String])), List()) = e.dt

    sb.situation shouldBe "b"
    sbw.situation shouldBe "bw"
    sa.situation shouldBe "a"
    saw.situation shouldBe "aw"
    sc.situation shouldBe "c"
    scw.situation shouldBe "cw"
    dlLogic.definedAt shouldBe Some(containsA)


    e("a") shouldBe "A"
    e("b") shouldBe "B"
    e("c") shouldBe "C"
    e("d") shouldBe "C"
  }

}
