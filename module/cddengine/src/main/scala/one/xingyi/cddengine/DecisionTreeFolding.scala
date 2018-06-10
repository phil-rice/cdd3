package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario
import one.xingyi.cddutilities.Lens

import scala.util.{Failure, Success, Try}

case class DecisionTreeFoldingData[P, R](st: DTFolderStrategy, lens: Lens[DecisionTreeNode[P, R], DecisionTreeNode[P, R]], fd: FolderData[P, R])

case class TraceData[P, R](tree: DecisionTree[P, R], s: Option[Scenario[P, R]], st: DTFolderStrategy)
object DecisionTreeFolder {
  def findStrategy[P, R](tree: DecisionTree[P, R], s: Scenario[P, R])(implicit folderStrategyFinder: DtFolderStrategyFinder[P, R]): Try[DecisionTreeFoldingData[P, R]] = {
    val (lens, lensCn) = tree.root.findLensAndCnLens(s.situation)
    val node: ConclusionNode[P, R] = lensCn(tree.root)
    val fd = FolderData(node, s)
    Try(folderStrategyFinder(fd)).map(s => DecisionTreeFoldingData(s, lens, fd))
  }

  def applyStrategy[P, R](tree: DecisionTree[P, R])(decisionTreeFoldingData: DecisionTreeFoldingData[P, R]) = {
    val newNode = decisionTreeFoldingData.st(decisionTreeFoldingData.fd)
    DecisionTree(decisionTreeFoldingData.lens.set(tree.root, newNode), tree.issues)
  }

  def recordError[P, R](tree: DecisionTree[P, R]): Throwable => DecisionTree[P, R] = {
    case e: DecisionIssue[_, _] => DecisionTree(tree.root, tree.issues :+ e.asInstanceOf[DecisionIssue[P, R]])
    case e => throw e
  }

  def findNewTree[P, R](tree: DecisionTree[P, R])(tryD: Try[DecisionTreeFoldingData[P, R]]) = tryD.map(applyStrategy(tree)).fold(recordError(tree), t => t)

  implicit def folder[P, R](implicit folderStrategyFinder: DtFolderStrategyFinder[P, R]): DecisionTreeFolder[P, R] = {
    (tree, s) => findNewTree(tree)(findStrategy[P, R](tree, s))
  }

  def apply[P, R](list: List[Scenario[P, R]])(implicit decisionTreeFolder: DecisionTreeFolder[P, R], default: DefaultFunction[P, R]): DecisionTree[P, R] =
    list.foldLeft[DecisionTree[P, R]](DecisionTree.empty)(decisionTreeFolder)

  def trace[P, R](list: List[Scenario[P, R]])(implicit decisionTreeFolder: DecisionTreeFolder[P, R]): List[TraceData[P, R]] =
    list.foldLeft[List[TraceData[P, R]]](List(TraceData(DecisionTree.empty, None, NullOp))) {
      case (list@(TraceData(tree, _, _) :: tail), s) =>
        val d = findStrategy(tree, s)
        val st = d.map(_.st).getOrElse(NullOp)
        val newTree = findNewTree(tree)(d)
        TraceData(newTree, Some(s), st) :: list
      case _ => ??? //??? suppresses warnings
    }


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
case object MakeDecisionNodeScenarioAsFalse extends DFFolderSimpleStrategy {
  def isDefinedAt[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): Boolean = c.logic.hasCondition && !c.accept(s)
  def apply[P, R](c: ConclusionNode[P, R], s: Scenario[P, R]): DecisionTreeNode[P, R] = DecisionNode(c.logic, ConclusionNode(List(s), s.logic), c)
}
case object MakeDecisionNodeScenarioAsTrue extends DTFolderStrategy {
  override def isDefinedAt[P, R](fd: FolderData[P, R]): Boolean = fd.scenario.logic.hasCondition && fd.sAcceptsFailUsingScenarioLogic.isEmpty && !fd.conclusionNode.logic.accept(fd.scenario)
  def apply[P, R](fd: FolderData[P, R]) = DecisionNode(fd.scenario.logic, fd.conclusionNode.copy(scenarios = fd.sRejects), ConclusionNode(fd.sAccepts, fd.scenario.logic))
}
case object ScenariosClash extends DTFolderStrategy {
  override def isDefinedAt[P, R](fd: FolderData[P, R]): Boolean = fd.sAcceptsFailUsingScenarioLogic.size > 0
  override def apply[P, R](fd: FolderData[P, R]): DecisionTreeNode[P, R] = throw new CannotAddScenarioBecauseClashes(fd.scenario, fd.sAcceptsFailUsingScenarioLogic)
}


object DtFolderStrategyFinder {
  implicit def defaultFinder[P, R]: DtFolderStrategyFinder[P, R] = new SimpleDtFolderStrategyFinder[P, R]
}

trait DtFolderStrategyFinder[P, R] extends (FolderData[P, R] => DTFolderStrategy)

class SimpleDtFolderStrategyFinder[P, R] extends DtFolderStrategyFinder[P, R] {

  type SS = PartialFunction[(ConclusionNode[P, R], Scenario[P, R]), DTFolderStrategy]

  val folders = List[DTFolderStrategy](
    AddScenarioToEmptyConclusion,
    AddScenarioReplaceLogic,
    AddScenarioMergeCondition,
    AddScenarioToConclusion,
    MakeDecisionNodeScenarioAsTrue,
    MakeDecisionNodeScenarioAsFalse,
    ScenariosClash)

  def apply(fd: FolderData[P, R]): DTFolderStrategy = folders.find(_.isDefinedAt(fd)).getOrElse(throw new RuntimeException(s"Cannot work out how to deal with $fd"))
}

