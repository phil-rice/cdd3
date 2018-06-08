package one.xingyi.cddcore
import one.xingyi.cddscenario._
import one.xingyi.cddutilities.DefinedInSourceCodeAt


trait DecisionTreeLanguage[P, R] {
  val cEmpty = ConclusionNode[P, R](List(), ScenarioLogic.empty)
  def s(p: P, r: R) = Scenario[P, R](p, ScenarioLogic.empty, List(), EngineComponentData(DefinedInSourceCodeAt.definedInSourceCodeAt(), Some(s"$p => $r")))
  def s(p: P, r: R, b: P => Boolean) = Scenario[P, R](p, SingleScenarioLogic[P, R](Some(r), Some(b), None, DefinedInSourceCodeAt.definedInSourceCodeAt(), ""), List(), EngineComponentData(DefinedInSourceCodeAt.definedInSourceCodeAt(), Some(s"$p => $r")))

  def c(s: Scenario[P, R], ss: Scenario[P, R]*) = ConclusionNode(s :: ss.toList, s.logic)
  def c(logic: ScenarioLogic[P, R], ss: Scenario[P, R]*) = ConclusionNode(ss.toList, logic)
  def d(right: Scenario[P, R], left: Scenario[P, R]) = DecisionNode(right.logic, c(left), c(right))
  def d(logic: ScenarioLogic[P, R], left: DecisionTreeNode[P, R], right: DecisionTreeNode[P, R]) = DecisionNode(logic, left, right)
  def d(logic: Scenario[P, R], left: DecisionTreeNode[P, R], right: DecisionTreeNode[P, R]) = DecisionNode(logic.logic, left, right)
  def t(dn: DecisionTreeNode[P, R]) = DecisionTree(dn, List())

}

trait DecisionTreeFixture extends ScenarioFixture with DecisionTreeLanguage[String, String] {


  val concNormal = c(snormal)
  val concNormal1And2 = c(snormal, snormal2)
  val concNoPassport = c(sNoPassport)
  val concGunNoPassport = c(sgunNoPassport)
  val concGun = c(sgun)
  val concGunGunNoPassport = c(sgun, sgunNoPassport)
  val dNormalPassport = d(sNoPassport.logic, concNormal, concNoPassport)
  val dNormalGun = d(sgun.logic, concNormal, concGun)
  val dGunNoPassword = d(sNoPassport.logic, concGun, concNoPassport)

  val conca = c(sa)
  val concawa = c(sawa)
  val concb = c(sb)
  val concbwb = c(sbwb)
}
