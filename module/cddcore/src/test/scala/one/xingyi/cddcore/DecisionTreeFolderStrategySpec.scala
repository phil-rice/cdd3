package one.xingyi.cddcore
import one.xingyi.cddutilities.CddSpec

import scala.reflect.ClassTag

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
    intercept[CannotAddScenarioBecauseClashes[P,R]](fold(fd))
  }


  behavior of fold.toString


}
class AddScenarioToEmptyConclusionSpec extends AbstractDtFolderStrategySpec(AddScenarioToEmptyConclusion) with DecisionTreeFixture {

  it should "accept a conclusion node that is empty and any scenario" in {
    isDefinedAt(concEmpty, sa) shouldBe true
    isDefinedAt(concEmpty, sa2) shouldBe true
    isDefinedAt(concEmpty, sab) shouldBe true
  }
  it should "not accept a conclusion not that has any scenarios" in {
    isDefinedAt(cn(sa), sab) shouldBe false
    isDefinedAt(cn(sab), sa) shouldBe false
    isDefinedAt(cn(sa2), sa) shouldBe false

  }

  it should "create a conclusion with the scenario" in {
    foldit(concEmpty, sa) shouldBe cn(sa)
    foldit(concEmpty, sa2) shouldBe cn(sa2)
  }
}

class AddScenarioToConclusionSpec extends AbstractDtFolderStrategySpec(AddScenarioToConclusion) with DecisionTreeFixture {

  it should "not accept an empty conclusion" in {
    isDefinedAt(concEmpty, sa) shouldBe false
    isDefinedAt(concEmpty, sa2) shouldBe false
    isDefinedAt(concEmpty, sab) shouldBe false

  }
  it should "accept when the scenario comes to the same result" in {
    isDefinedAt(cn(sa), saba) shouldBe true
    isDefinedAt(cn(sa), sawa) shouldBe true
    isDefinedAt(cn(saba), sa) shouldBe true
    isDefinedAt(cn(saba), sawa) shouldBe true
    isDefinedAt(cn(sawa), sa) shouldBe true
    isDefinedAt(cn(sawa), sawa) shouldBe true
  }

  it should "not accept when comes to a different result" in {
    isDefinedAt(cn(sa), sb) shouldBe false
    isDefinedAt(cn(saba), sb) shouldBe false
    isDefinedAt(cn(sawa), sb) shouldBe false

  }

  it should "create a conclusion with the scenario" in {
    foldit(cn(sa), saba) shouldBe ConclusionNode(List(sa, saba), sa.logic) // note that this isn't logic we want to happen. AddScenarioReplaceLogic deals with that
    foldit(cn(saba), sa) shouldBe ConclusionNode(List(saba, sa), saba.logic)
  }
}

class AddScenarioReplaceLogicSpec extends AbstractDtFolderStrategySpec(AddScenarioReplaceLogic) with DecisionTreeFixture {

  it should "not accept an empty conclusion" in {
    isDefinedAt(concEmpty, sa) shouldBe false
    isDefinedAt(concEmpty, sa2) shouldBe false
    isDefinedAt(concEmpty, sab) shouldBe false

  }
  it should "accept when the scenario comes to the same conclusion and there is no condition in the conclusion" in {
    isDefinedAt(cn(sa), saba) shouldBe true
    isDefinedAt(cn(sa), sawa) shouldBe true
  }

  it should "not accept when conclusion has condition" in {
    isDefinedAt(cn(saba), sa) shouldBe false
    isDefinedAt(cn(saba), sawa) shouldBe false
    isDefinedAt(cn(sawa), sa) shouldBe false
    isDefinedAt(cn(sawa), sawa) shouldBe false

  }

  it should "not accept when comes to a different result" in {
    isDefinedAt(cn(sa), sb) shouldBe false
  }

  it should "create a conclusion with the scenario replacing the logic" in {
    foldit(cn(sa), saba) shouldBe ConclusionNode(List(sa, saba), saba.logic)
  }
}

class AddScenarioMergeConditionSpec extends AbstractDtFolderStrategySpec(AddScenarioMergeCondition) with DecisionTreeFixture {

  it should "not accept an empty conclusion" in {
    isDefinedAt(concEmpty, sa) shouldBe false
    isDefinedAt(concEmpty, sa2) shouldBe false
    isDefinedAt(concEmpty, sab) shouldBe false

  }
  it should "accept when the scenario comes to the same conclusion and there is a condition in the conclusion and scenario" in {
    isDefinedAt(cn(saba), sawa) shouldBe true
    isDefinedAt(cn(sawa), saba) shouldBe true
  }

  it should "not accept when conclusion has no condition" in {
    isDefinedAt(cn(sa), sa2) shouldBe false
    isDefinedAt(cn(sa), saba) shouldBe false
    isDefinedAt(cn(sa), sawa) shouldBe false

  }
  it should "not accept when scenario has no condition" in {
    isDefinedAt(cn(saba), sa) shouldBe false
    isDefinedAt(cn(sawa), sa) shouldBe false
  }

  it should "not accept when comes to a different result" in {
    isDefinedAt(cn(saba), sbwb) shouldBe false
    isDefinedAt(cn(saba), sb) shouldBe false
    isDefinedAt(cn(sa), sbwb) shouldBe false
  }

  it should "create a conclusion with the scenario merging the logic" in {
    foldit(cn(saba), sawa) shouldBe ConclusionNode(List(saba, sawa), saba.logic or sawa.logic)
  }
}

class MakeDecisionNodeScenarioOnLeftSpec extends AbstractDtFolderStrategySpec(MakeDecisionNodeScenarioOnLeft) with DecisionTreeFixture {

  it should "not accept an empty conclusion" in {
    isDefinedAt(concEmpty, sa) shouldBe false
    isDefinedAt(concEmpty, sa2) shouldBe false
    isDefinedAt(concEmpty, sab) shouldBe false

  }
  it should "reject if the conclusion and scenario come to same result" in {
    isDefinedAt(cn(sa), saba) shouldBe false
    isDefinedAt(cn(sa), sawa) shouldBe false
    isDefinedAt(cn(saba), sa) shouldBe false
    isDefinedAt(cn(saba), sawa) shouldBe false
    isDefinedAt(cn(sawa), sa) shouldBe false
    isDefinedAt(cn(sawa), sawa) shouldBe false
  }

  it should "not accept when conclusion has no condition" in {
    isDefinedAt(cn(sa), sb) shouldBe false
    isDefinedAt(cn(sa), sbbb) shouldBe false
    isDefinedAt(cn(sa), sbwb) shouldBe false

  }
  it should " accept when conclusion and scenario have different result (as long as conclusion has condition)" in {
    isDefinedAt(cn(saba), sb) shouldBe true
    isDefinedAt(cn(saba), sbbb) shouldBe true
    isDefinedAt(cn(saba), sbwb) shouldBe true

    isDefinedAt(cn(sawa), sb) shouldBe true
    isDefinedAt(cn(sawa), sbbb) shouldBe true
    isDefinedAt(cn(sawa), sbwb) shouldBe true
  }

  it should "create a decision node  " in {
    foldit(cn(saba), sb) shouldBe DecisionNode(saba.logic, cn(sb), cn(saba))
    foldit(cn(sawa), sb) shouldBe DecisionNode(sawa.logic, cn(sb), cn(sawa))
  }
}

class MakeDecisionNodeScenarioOnRightSpec extends AbstractDtFolderStrategySpec(MakeDecisionNodeScenarioOnRight) with DecisionTreeFixture {

  it should "not accept an empty conclusion" in {
    isDefinedAt(concEmpty, sa) shouldBe false
    isDefinedAt(concEmpty, sa2) shouldBe false
    isDefinedAt(concEmpty, sab) shouldBe false

  }
  it should "reject if the conclusion and scenario come to same result" in {
    isDefinedAt(cn(sa), saba) shouldBe false
    isDefinedAt(cn(sa), sawa) shouldBe false
    isDefinedAt(cn(saba), sa) shouldBe false
    isDefinedAt(cn(saba), sawa) shouldBe false
    isDefinedAt(cn(sawa), sa) shouldBe false
    isDefinedAt(cn(sawa), sawa) shouldBe false
  }

  it should "not accept when scenario has no condition" in {
    isDefinedAt(cn(sa), sb) shouldBe false
    isDefinedAt(cn(saba), sb) shouldBe false
    isDefinedAt(cn(sawa), sb) shouldBe false

  }
  it should " accept when conclusion and scenario have different result (as long as scenario has condition)" in {
    isDefinedAt(cn(sa), sbbb) shouldBe true
    isDefinedAt(cn(sa), sbwb) shouldBe true

    isDefinedAt(cn(saba), sbbb) shouldBe true
    isDefinedAt(cn(saba), sbwb) shouldBe true

    isDefinedAt(cn(sawa), sbbb) shouldBe true
    isDefinedAt(cn(sawa), sbwb) shouldBe true
  }

  it should "create a decision node  " in {
    foldit(cn(sa), sbbb) shouldBe DecisionNode(sbbb.logic, cn(sa), cn(sbbb))
    foldit(cn(saba), sbbb) shouldBe DecisionNode(sbbb.logic, cn(saba), cn(sbbb))
    foldit(cn(sawa), sbbb) shouldBe DecisionNode(sbbb.logic, cn(sawa), cn(sbbb))
  }

}
class ScenariosClashSpec extends AbstractDtFolderStrategySpec(ScenariosClash) with DecisionTreeFixture {

  it should "not accept an empty conclusion" in {
    isDefinedAt(concEmpty, sa) shouldBe false
    isDefinedAt(concEmpty, sa2) shouldBe false
    isDefinedAt(concEmpty, sab) shouldBe false

  }
  it should "reject if the conclusion and scenario come to same result" in {
    isDefinedAt(cn(sa), saba) shouldBe false
    isDefinedAt(cn(sa), sawa) shouldBe false
    isDefinedAt(cn(saba), sa) shouldBe false
    isDefinedAt(cn(saba), sawa) shouldBe false
    isDefinedAt(cn(sawa), sa) shouldBe false
    isDefinedAt(cn(sawa), sawa) shouldBe false
  }


  it should "throw  a decision node  " in {
//    foldit(cn(sbbb), sabBecomesA) shouldBe ""
    foldClashes(cn(sbbb), sabBecomesA) shouldBe CannotAddScenarioBecauseClashes(sabBecomesA, List(sbbb))
  }

}
