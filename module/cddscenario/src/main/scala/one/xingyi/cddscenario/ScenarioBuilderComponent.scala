package one.xingyi.cddscenario

import java.util.concurrent.atomic.AtomicInteger

import one.xingyi.cddutilities.{DefinedInSourceCodeAt, SingleDefinedInSourceCodeAt}

import scala.annotation.implicitNotFound


trait IdMaker {
  private val nextId = new AtomicInteger()
  protected def getNextId = nextId.getAndIncrement()

}

object UntypedScenarioBuilder extends UntypedScenarioBuilder
trait UntypedScenarioBuilder extends IdMaker {
  def scenario[P](p: P): RawSituation[P] = RawSituation(p, ScenarioBuilderData(getNextId, p))
  def scenario[P1, P2](p1: P1, p2: P2): RawSituation[(P1, P2)] = RawSituation((p1, p2), ScenarioBuilderData(getNextId, (p1, p2)))
  def scenario[P1, P2, P3](p1: P1, p2: P2, p3: P3): RawSituation[(P1, P2, P3)] = RawSituation((p1, p2, p3), ScenarioBuilderData(getNextId, (p1, p2, p3)))
  def scenario[P1, P2, P3, P4](p1: P1, p2: P2, p3: P3, p4: P4): RawSituation[(P1, P2, P3, P4)] = RawSituation((p1, p2, p3, p4), ScenarioBuilderData(getNextId, (p1, p2, p3, p4)))
  def scenario[P1, P2, P3, P4, P5](p1: P1, p2: P2, p3: P3, p4: P4, p5: P5): RawSituation[(P1, P2, P3, P4, P5)] = RawSituation((p1, p2, p3, p4, p5), ScenarioBuilderData(getNextId, (p1, p2, p3, p4, p5)))

}


case class ScenarioBuilderData[P, R](id: Int, situation: P, result: Option[R] = None, title: Option[String] = None, comment: Option[String] = None, assertions: List[ScenarioAssertion[P, R]] = List(),
                                     references: List[Reference] = List(), isDefinedAt: SingleDefinedInSourceCodeAt = DefinedInSourceCodeAt.definedInSourceCodeAt()) {
  def data: EngineComponentData = EngineComponentData(isDefinedAt, title, comment, List(), references)
}


// yeah I know. This is all about the side effects. I don't know how to make a DSL with nice error messages that doesn't have side effects
@implicitNotFound("""If you are making the scenario using a use case you will have one of these. If you NEED to have no aggregator and know what you are doing, you can import the NullScenarioAggregator""")
trait ScenarioAggregator[P, R] extends (ScenarioBuilderComponent[_, P, R] => Unit)
class RememberingScenarioAggregator[P, R] extends ScenarioAggregator[P, R] {
  private var list = List[ScenarioBuilderComponent[_, P, R]]()
  private val lock = new Object()
  override def apply(comp: ScenarioBuilderComponent[_, P, R]): Unit = lock.synchronized(
    list = list.filterNot(_.data.id == comp.data.id) :+ comp
  )
  def withAggreator[X](fn: ScenarioAggregator[P, R] => X): (X, List[Scenario[P, R]]) = {
    val x = fn(this)
    (x, list.map(_.scenario))
  }
  def scenarios = list.map(_.scenario)
}

object NullScenarioAggregator {
  implicit def nullAggregator[P, R]: ScenarioAggregator[P, R] = scenario => {}
}

case class RawSituation[P](p: P, data: ScenarioBuilderData[P, Nothing]) {
  def produces[R](r: R)(implicit a: ScenarioAggregator[P, R]): Produces[P, R] = {
    require(data.assertions.size == 0)
    Produces[P, R](data.copy(result = Some(r), assertions = List()))
  }
}
case class Produces[P, R](data: ScenarioBuilderData[P, R])(implicit a: ScenarioAggregator[P, R]) extends ScenarioBuilderComponent[Produces[P, R], P, R] {
  override protected def rawCopyWith(fn: ScenarioBuilderData[P, R] => ScenarioBuilderData[P, R]) = copy(data = data)
  def when(whenFn: P => Boolean) = WithWhen(whenFn, data)
//  def when2[P1,P2](whenFn: (P1,P2) => Boolean)(implicit evidence: (P1, P2) =:= P) = WithWhen(whenFn.tupled, data)
  def because(whenFn: PartialFunction[P, R]) = WithBecause(whenFn, data)
  override val scenarioReason: SingleScenarioLogic[P, R] = SingleScenarioLogic[P, R](data.result, None, None, data.isDefinedAt)
}

case class WithWhen[P, R](whenFn: P => Boolean, data: ScenarioBuilderData[P, R])(implicit a: ScenarioAggregator[P, R]) extends ScenarioBuilderComponent[WithWhen[P, R], P, R] {
  override protected def rawCopyWith(fn: ScenarioBuilderData[P, R] => ScenarioBuilderData[P, R]) = copy(data = data)
  override val scenarioReason: SingleScenarioLogic[P, R] = SingleScenarioLogic(data.result, Some(whenFn), None, data.isDefinedAt)
}
case class WithBecause[P, R](becauseFn: PartialFunction[P, R], data: ScenarioBuilderData[P, R])(implicit a: ScenarioAggregator[P, R]) extends ScenarioBuilderComponent[WithBecause[P, R], P, R] {
  override protected def rawCopyWith(fn: ScenarioBuilderData[P, R] => ScenarioBuilderData[P, R]) = copy(data = data)
  override val scenarioReason: SingleScenarioLogic[P, R] = SingleScenarioLogic(data.result, Some(becauseFn.isDefinedAt), Some(becauseFn.apply), data.isDefinedAt)
}

abstract class ScenarioBuilderComponent[Self <: ScenarioBuilderComponent[Self, P, R], P, R](implicit a: ScenarioAggregator[P, R]) {
  a.apply(this)
  import one.xingyi.cddutilities.Arrows._

  def scenarioReason: SingleScenarioLogic[P, R]
  def scenario: Scenario[P, R] = Scenario(data.situation, scenarioReason, data.assertions, data.data)
  def data: ScenarioBuilderData[P, R]
  protected def copyWith(fn: ScenarioBuilderData[P, R] => ScenarioBuilderData[P, R])(implicit a: ScenarioAggregator[P, R]): Self = rawCopyWith _ sideeffect a apply fn
  protected def rawCopyWith(fn: ScenarioBuilderData[P, R] => ScenarioBuilderData[P, R]): Self
  def comment(string: String): Self = copyWith(_.copy(comment = Some(string)))
  def title(string: String): Self = copyWith(_.copy(title = Some(string)))
  def reference(reference: Reference): Self = copyWith(d => d.copy(references = d.references :+ reference))
}

