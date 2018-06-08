package one.xingyi.cddcore
import one.xingyi.cddscenario.Scenario
import one.xingyi.cddscenario.ScenarioLogic.CompositeScenarioLogic
import one.xingyi.cddutilities.CddSpec

class AbstractDtFolderStrategySpec(val fold: DTFolderStrategy) extends CddSpec with DecisionTreeFixture {

  def isDefinedAt[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]) = fold.isDefinedAt(FolderData(c, s))
  def foldit[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]) = {
    val fd = FolderData(c, s)
    isDefinedAt(c, s) shouldBe true
    fold(fd)
  }
  def foldClashes[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]) = {
    val fd = FolderData(c, s)
    isDefinedAt(c, s) shouldBe true
    intercept[CannotAddScenarioBecauseClashes[P, R]](fold(fd))
  }


  behavior of fold.toString


}
class AddScenarioToEmptyConclusionSpec extends AbstractDtFolderStrategySpec(AddScenarioToEmptyConclusion) with DecisionTreeFixture {

  it should "accept a conclusion node that is empty and any scenario" in {
    isDefinedAt(cEmpty, sa) shouldBe true
    isDefinedAt(cEmpty, sa2) shouldBe true
    isDefinedAt(cEmpty, sab) shouldBe true
  }
  it should "not accept a conclusion not that has any scenarios" in {
    isDefinedAt(c(sa), sab) shouldBe false
    isDefinedAt(c(sab), sa) shouldBe false
    isDefinedAt(c(sa2), sa) shouldBe false

  }

  it should "create a conclusion with the scenario" in {
    foldit(cEmpty, sa) shouldBe c(sa)
    foldit(cEmpty, sa2) shouldBe c(sa2)
  }
}

class AddScenarioToConclusionSpec extends AbstractDtFolderStrategySpec(AddScenarioToConclusion) with DecisionTreeFixture {

  it should "not accept an empty conclusion" in {
    isDefinedAt(cEmpty, sa) shouldBe false
    isDefinedAt(cEmpty, sa2) shouldBe false
    isDefinedAt(cEmpty, sab) shouldBe false

  }
  it should "accept when the scenario comes to the same result" in {
    isDefinedAt(c(sa), saba) shouldBe true
    isDefinedAt(c(sa), sawa) shouldBe true
    isDefinedAt(c(saba), sa) shouldBe true
    isDefinedAt(c(saba), sawa) shouldBe true
    isDefinedAt(c(sawa), sa) shouldBe true
    isDefinedAt(c(sawa), sawa) shouldBe true
  }

  it should "not accept when comes to a different result" in {
    isDefinedAt(c(sa), sb) shouldBe false
    isDefinedAt(c(saba), sb) shouldBe false
    isDefinedAt(c(sawa), sb) shouldBe false

  }

  it should "create a conclusion with the scenario" in {
    foldit(c(sa), saba) shouldBe ConclusionNode(List(sa, saba), sa.logic) // note that this isn't logic we want to happen. AddScenarioReplaceLogic deals with that
    foldit(c(saba), sa) shouldBe ConclusionNode(List(saba, sa), saba.logic)
  }
}

class AddScenarioReplaceLogicSpec extends AbstractDtFolderStrategySpec(AddScenarioReplaceLogic) with DecisionTreeFixture {

  it should "not accept an empty conclusion" in {
    isDefinedAt(cEmpty, sa) shouldBe false
    isDefinedAt(cEmpty, sa2) shouldBe false
    isDefinedAt(cEmpty, sab) shouldBe false

  }
  it should "accept when the scenario comes to the same conclusion and there is no condition in the conclusion" in {
    isDefinedAt(c(sa), saba) shouldBe true
    isDefinedAt(c(sa), sawa) shouldBe true
  }

  it should "not accept when conclusion has condition" in {
    isDefinedAt(c(saba), sa) shouldBe false
    isDefinedAt(c(saba), sawa) shouldBe false
    isDefinedAt(c(sawa), sa) shouldBe false
    isDefinedAt(c(sawa), sawa) shouldBe false

  }

  it should "not accept when comes to a different result" in {
    isDefinedAt(c(sa), sb) shouldBe false
  }

  it should "create a conclusion with the scenario replacing the logic" in {
    foldit(c(sa), saba) shouldBe ConclusionNode(List(sa, saba), saba.logic)
  }
}

class AddScenarioMergeConditionSpec extends AbstractDtFolderStrategySpec(AddScenarioMergeCondition) with DecisionTreeFixture {

  it should "not accept an empty conclusion" in {
    isDefinedAt(cEmpty, sa) shouldBe false
    isDefinedAt(cEmpty, sa2) shouldBe false
    isDefinedAt(cEmpty, sab) shouldBe false

  }
  it should "accept when the scenario comes to the same conclusion and there is a condition in the conclusion and scenario" in {
    isDefinedAt(c(saba), sawa) shouldBe true
    isDefinedAt(c(sawa), saba) shouldBe true
  }

  it should "not accept when conclusion has no condition" in {
    isDefinedAt(c(sa), sa2) shouldBe false
    isDefinedAt(c(sa), saba) shouldBe false
    isDefinedAt(c(sa), sawa) shouldBe false

  }
  it should "not accept when scenario has no condition" in {
    isDefinedAt(c(saba), sa) shouldBe false
    isDefinedAt(c(sawa), sa) shouldBe false
  }

  it should "not accept when comes to a different result" in {
    isDefinedAt(c(saba), sbwb) shouldBe false
    isDefinedAt(c(saba), sb) shouldBe false
    isDefinedAt(c(sa), sbwb) shouldBe false
  }

  it should "create a conclusion with the scenario merging the logic" in {

    foldit(c(saba), sawa) shouldBe ConclusionNode(List(saba, sawa), CompositeScenarioLogic(Seq(saba.logic, sawa.logic)))
  }
}

class MakeDecisionNodeScenarioOnLeftSpec extends AbstractDtFolderStrategySpec(MakeDecisionNodeScenarioOnLeft) with DecisionTreeFixture {

  it should "not accept an empty conclusion" in {
    isDefinedAt(cEmpty, sa) shouldBe false
    isDefinedAt(cEmpty, sa2) shouldBe false
    isDefinedAt(cEmpty, sab) shouldBe false

  }
  it should "reject if the conclusion and scenario come to same result" in {
    isDefinedAt(c(sa), saba) shouldBe false
    isDefinedAt(c(sa), sawa) shouldBe false
    isDefinedAt(c(saba), sa) shouldBe false
    isDefinedAt(c(saba), sawa) shouldBe false
    isDefinedAt(c(sawa), sa) shouldBe false
    isDefinedAt(c(sawa), sawa) shouldBe false
  }

  it should "not accept when conclusion has no condition" in {
    isDefinedAt(c(sa), sb) shouldBe false
    isDefinedAt(c(sa), sbbb) shouldBe false
    isDefinedAt(c(sa), sbwb) shouldBe false

  }
  it should " accept when conclusion and scenario have different result (as long as conclusion has condition)" in {
    isDefinedAt(c(saba), sb) shouldBe true
    isDefinedAt(c(saba), sbbb) shouldBe true
    isDefinedAt(c(saba), sbwb) shouldBe true

    isDefinedAt(c(sawa), sb) shouldBe true
    isDefinedAt(c(sawa), sbbb) shouldBe true
    isDefinedAt(c(sawa), sbwb) shouldBe true
  }

  it should "create a decision node  " in {
    foldit(c(saba), sb) shouldBe DecisionNode(saba.logic, c(sb), c(saba))
    foldit(c(sawa), sb) shouldBe DecisionNode(sawa.logic, c(sb), c(sawa))
  }
}

class MakeDecisionNodeScenarioOnRightSpec extends AbstractDtFolderStrategySpec(MakeDecisionNodeScenarioOnRight) with DecisionTreeFixture {

  it should "not accept an empty conclusion" in {
    isDefinedAt(cEmpty, sa) shouldBe false
    isDefinedAt(cEmpty, sa2) shouldBe false
    isDefinedAt(cEmpty, sab) shouldBe false

  }
  it should "reject if the conclusion and scenario come to same result" in {
    isDefinedAt(c(sa), saba) shouldBe false
    isDefinedAt(c(sa), sawa) shouldBe false
    isDefinedAt(c(saba), sa) shouldBe false
    isDefinedAt(c(saba), sawa) shouldBe false
    isDefinedAt(c(sawa), sa) shouldBe false
    isDefinedAt(c(sawa), sawa) shouldBe false
  }

  it should "not accept when scenario has no condition" in {
    isDefinedAt(c(sa), sb) shouldBe false
    isDefinedAt(c(saba), sb) shouldBe false
    isDefinedAt(c(sawa), sb) shouldBe false

  }
  it should " accept when conclusion and scenario have different result (as long as scenario has condition)" in {
    isDefinedAt(c(sa), sbbb) shouldBe true
    isDefinedAt(c(sa), sbwb) shouldBe true

    isDefinedAt(c(saba), sbbb) shouldBe true
    isDefinedAt(c(saba), sbwb) shouldBe true

    isDefinedAt(c(sawa), sbbb) shouldBe true
    isDefinedAt(c(sawa), sbwb) shouldBe true
  }

  it should "create a decision node  " in {
    foldit(c(sa), sbbb) shouldBe DecisionNode(sbbb.logic, c(sa), c(sbbb))
    foldit(c(saba), sbbb) shouldBe DecisionNode(sbbb.logic, c(saba), c(sbbb))
    foldit(c(sawa), sbbb) shouldBe DecisionNode(sbbb.logic, c(sawa), c(sbbb))
  }

}
class ScenariosClashSpec extends AbstractDtFolderStrategySpec(ScenariosClash) with DecisionTreeFixture {

  it should "not accept an empty conclusion" in {
    isDefinedAt(cEmpty, sa) shouldBe false
    isDefinedAt(cEmpty, sa2) shouldBe false
    isDefinedAt(cEmpty, sab) shouldBe false

  }
  it should "reject if the conclusion and scenario come to same result" in {
    isDefinedAt(c(sa), saba) shouldBe false
    isDefinedAt(c(sa), sawa) shouldBe false
    isDefinedAt(c(saba), sa) shouldBe false
    isDefinedAt(c(saba), sawa) shouldBe false
    isDefinedAt(c(sawa), sa) shouldBe false
    isDefinedAt(c(sawa), sawa) shouldBe false
  }


  it should "throw  a decision node  " in {
    //    foldit(c(sbbb), sabBecomesA) shouldBe ""
    foldClashes(c(sbbb), sabBecomesA) shouldBe CannotAddScenarioBecauseClashes(sabBecomesA, List(sbbb))
  }

}
