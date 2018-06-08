package one.xingyi.cddscenario

import scala.language.experimental.macros
import scala.reflect.macros.blackbox


case class RawSituation[P](p: P, data: ScenarioBuilderData[P, Nothing]) {
  def produces[R](r: R)(implicit a: ScenarioAggregator[P, R]): Produces[P, R] = {
    require(data.assertions.size == 0)
    Produces[P, R](data.copy(result = Some(r), assertions = List()))
  }
}

case class Produces[P, R](data: ScenarioBuilderData[P, R])(implicit val a: ScenarioAggregator[P, R]) extends ScenarioBuilderComponent[Produces[P, R], P, R] {
  override protected def rawCopyWith(fn: ScenarioBuilderData[P, R] => ScenarioBuilderData[P, R]) = copy(data = data)
  def when(whenFn: P => Boolean): WithWhen[P, R] = macro Produces.when_impl[P, R]
  def because(whenFn: PartialFunction[P, R]) = WithBecause(whenFn, data)
  override val scenarioReason: SingleScenarioLogic[P, R] = SingleScenarioLogic[P, R](data.result, None, None, data.isDefinedAt, "Produces hasn't got this yet")
}

object Produces {
  def when_impl[P: c.WeakTypeTag, R: c.WeakTypeTag](c: blackbox.Context)(whenFn: c.Expr[P => Boolean]): c.Expr[WithWhen[P, R]] = {
    import c.universe._
    reify {
      val produces = (c.Expr[Produces[P, R]](c.prefix.tree)).splice
      WithWhen[P, R](whenFn.splice, produces.data, c.literal(show(whenFn.tree)).splice)(produces.a)
    }
  }
}

case class WithWhen[P, R](whenFn: P => Boolean, data: ScenarioBuilderData[P, R], ifString: String)(implicit a: ScenarioAggregator[P, R]) extends ScenarioBuilderComponent[WithWhen[P, R], P, R] {
//  println(s"WithWhen: $ifString")
  override protected def rawCopyWith(fn: ScenarioBuilderData[P, R] => ScenarioBuilderData[P, R]) = copy(data = data)
  override val scenarioReason: SingleScenarioLogic[P, R] = SingleScenarioLogic(data.result, Some(whenFn), None, data.isDefinedAt, ifString)
}
case class WithBecause[P, R](becauseFn: PartialFunction[P, R], data: ScenarioBuilderData[P, R])(implicit a: ScenarioAggregator[P, R]) extends ScenarioBuilderComponent[WithBecause[P, R], P, R] {
  override protected def rawCopyWith(fn: ScenarioBuilderData[P, R] => ScenarioBuilderData[P, R]) = copy(data = data)
  override val scenarioReason: SingleScenarioLogic[P, R] = SingleScenarioLogic(data.result, Some(becauseFn.isDefinedAt), Some(becauseFn.apply), data.isDefinedAt, "Because hasn't got this yet")
}



