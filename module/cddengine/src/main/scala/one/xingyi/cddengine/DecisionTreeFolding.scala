package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario

import scala.util.{Failure, Success, Try}

object DecisionTreeFolder {
  implicit def folder[P, R](implicit conclusionNodeEditor: ConclusionAndScenarioStrategyFinder[P, R]): DecisionTreeFolder[P, R] = { (tree, s) =>
    val (lens, lensCn) = tree.root.findLensAndCnLens(s.situation)
    Try(conclusionNodeEditor.makeReplacementNode apply(lensCn(tree.root), s)) match {
      case Success(node) => DecisionTree(lens.set(tree.root, node), tree.issues)
      case Failure(e: DecisionIssue[_, _]) => DecisionTree(tree.root, tree.issues :+ e.asInstanceOf[DecisionIssue[P, R]])
      case Failure(e) => throw e
    }
  }

  def apply[P, R](list: List[Scenario[P, R]])(implicit decisionTreeFolder: DecisionTreeFolder[P, R], default: DefaultFunction[P, R]): DecisionTree[P, R] =
    list.foldLeft[DecisionTree[P, R]](DecisionTree.empty)(decisionTreeFolder)

  def trace[P, R](list: List[Scenario[P, R]])(implicit decisionTreeFolder: DecisionTreeFolder[P, R]): List[DecisionTree[P, R]] =
    list.foldLeft[List[DecisionTree[P, R]]](List(DecisionTree.empty)) { case (list@(head :: tail), s) => decisionTreeFolder(head, s) :: list; case _ => ??? } //??? suppresses warnings


}
trait DecisionTreeFolder[P, R] extends ((DecisionTree[P, R], Scenario[P, R]) => DecisionTree[P, R])


object FolderData {
  def create[P, R]: (ConclusionNode[P, R], Scenario[P, R]) => FolderData[P, R] = (c, s) => new FolderData(c, s)
}

case class FolderData[P, R](conclusionNode: ConclusionNode[P, R], scenario: Scenario[P, R]) {
  private def findFails(list: List[Scenario[P, R]])(code: P => R) = list.filterNot(s => Try(s.acceptResult(s.situation, code(s.situation))) == Success(true))
  lazy val (sAccepts, sRejects) = (scenario :: conclusionNode.scenarios).partition(s => scenario.logic.fn.isDefinedAt(s.situation))
  lazy val (cAccepts, cRejects) = (scenario :: conclusionNode.scenarios).partition(s => conclusionNode.logic.fn.isDefinedAt(s.situation))
  lazy val emptyScenarios = List[Scenario[P, R]]()
  lazy val sAcceptsFailUsingScenarioLogic = findFails(sAccepts)(scenario.logic.fn)
}


sealed trait DTFolderStrategy {
  def isDefinedAt[P, R](fd: FolderData[P, R]): Boolean
  def apply[P, R](fd: FolderData[P, R]): DecisionTreeNode[P, R]

}
trait DFFolderSimpleStrategy extends DTFolderStrategy {
  def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): DecisionTreeNode[P, R]
  def isDefinedAt[P, R](conclusionNode: ConclusionNode[P, R], scenario: Scenario[P, R]): Boolean

  def isDefinedAt[P, R](fd: FolderData[P, R]): Boolean = isDefinedAt(fd.conclusionNode, fd.scenario)
  def apply[P, R](fd: FolderData[P, R]): DecisionTreeNode[P, R] = apply(fd.conclusionNode, fd.scenario)
}

case object NullOp extends DFFolderSimpleStrategy {
  override def isDefinedAt[P, R](conclusionNode: ConclusionNode[P, R], scenario: Scenario[P, R]): Boolean = false
  override def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): DecisionTreeNode[P, R] = throw new IllegalStateException("should not be called")
}

case object AddScenarioToEmptyConclusion extends DFFolderSimpleStrategy {
  def isDefinedAt[P, R](conclusionNode: ConclusionNode[P, R], scenario: Scenario[P, R]): Boolean = conclusionNode.scenarios.isEmpty
  def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): DecisionTreeNode[P, R] = c.copy(scenarios = c.scenarios :+ s, logic = s.logic)
}
case object AddScenarioToConclusion extends DFFolderSimpleStrategy {
  def isDefinedAt[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): Boolean = c.accept(s)
  def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): DecisionTreeNode[P, R] = c.copy(scenarios = c.scenarios :+ s)
}
case object AddScenarioReplaceLogic extends DFFolderSimpleStrategy {
  override def isDefinedAt[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): Boolean = !c.logic.hasCondition && s.logic.hasCondition && c.scenarios.forall(s.logic.accept)
  def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): DecisionTreeNode[P, R] = c.copy(scenarios = c.scenarios :+ s, logic = s.logic)
}
case object AddScenarioMergeCondition extends DFFolderSimpleStrategy {
  import one.xingyi.cddutilities.SemiGroupLanguage._
  def isDefinedAt[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): Boolean = c.logic.hasCondition && s.logic.hasCondition && c.accept(s)
  def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): DecisionTreeNode[P, R] = c.copy(scenarios = c.scenarios :+ s, logic = c.logic or s.logic)
}
case object MakeDecisionNodeScenarioOnLeft extends DFFolderSimpleStrategy {
  def isDefinedAt[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): Boolean = c.logic.hasCondition && !c.accept(s)
  def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): DecisionTreeNode[P, R] = DecisionNode(c.logic, ConclusionNode(List(s), s.logic), c)
}
case object MakeDecisionNodeScenarioOnRight extends DTFolderStrategy {
  override def isDefinedAt[P, R](fd: FolderData[P, R]): Boolean = fd.scenario.logic.hasCondition && fd.sAcceptsFailUsingScenarioLogic.isEmpty && !fd.conclusionNode.logic.accept(fd.scenario)
  def apply[P, R](fd: FolderData[P, R]) = DecisionNode(fd.scenario.logic, fd.conclusionNode.copy(scenarios = fd.sRejects), ConclusionNode(fd.sAccepts, fd.scenario.logic))
}
case object ScenariosClash extends DTFolderStrategy {
  override def isDefinedAt[P, R](fd: FolderData[P, R]): Boolean = fd.sAcceptsFailUsingScenarioLogic.size > 0
  override def apply[P, R](fd: FolderData[P, R]): DecisionTreeNode[P, R] = throw new CannotAddScenarioBecauseClashes(fd.scenario, fd.sAcceptsFailUsingScenarioLogic)
}


object ConclusionAndScenarioStrategyFinder {
  implicit def defaultEditor[P, R] = new ConclusionAndScenarioStrategyFinder[P, R]
}

class ConclusionAndScenarioStrategyFinder[P, R] {

  type SS = PartialFunction[(ConclusionNode[P, R], Scenario[P, R]), DTFolderStrategy]

  val folders = List[DTFolderStrategy](
    AddScenarioToEmptyConclusion,
    AddScenarioReplaceLogic,
    AddScenarioMergeCondition,
    AddScenarioToConclusion,
    MakeDecisionNodeScenarioOnLeft,
    MakeDecisionNodeScenarioOnRight,
    ScenariosClash)

  def findStrategy(fd: FolderData[P, R]) = folders.find(_.isDefinedAt(fd)).getOrElse(throw new RuntimeException(s"Cannot work out how to deal with $fd"))
  def makeReplacementNode: (ConclusionNode[P, R], Scenario[P, R]) => DecisionTreeNode[P, R] = { (c, s) => val fd = FolderData(c, s); findStrategy(fd)(fd) }

}
