package one.xingyi.cddscenario

import one.xingyi.cddutilities.CodeHolder

import scala.language.experimental.macros
import scala.reflect.macros.blackbox


case class RawSituation2[P1, P2](p1: P1, p2: P2, data: ScenarioBuilderData[(P1, P2), Nothing]) {
  def produces[R](r: R)(implicit a: ScenarioAggregator[(P1, P2), R]): Produces2[P1, P2, R] = {
    require(data.assertions.isEmpty)
    Produces2[P1, P2, R](data.copy(result = Some(r), assertions = List()))
  }
}

case class Produces2[P1, P2, R](data: ScenarioBuilderData[(P1, P2), R])(implicit val a: ScenarioAggregator[(P1, P2), R]) extends ScenarioBuilderComponent[Produces2[P1, P2, R], (P1, P2), R] {
  override protected def rawCopyWith(fn: ScenarioBuilderData[(P1, P2), R] => ScenarioBuilderData[(P1, P2), R]): Produces2[P1, P2, R] = copy(data = data)
  def when(whenFn: (P1, P2) => Boolean): WithWhen2[P1, P2, R] = macro Produces2.when_impl[P1, P2, R]
  def because(becauseFn: PartialFunction[(P1, P2), R]) = macro Produces2.because_impl[P1, P2, R]
  override val scenarioReason: SingleScenarioLogic[(P1, P2), R] = SingleScenarioLogic[(P1, P2), R](data.result, None, None, data.isDefinedAt, "Produces hasn't got this yet")
}

object Produces2 {
  def when_impl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag](c: blackbox.Context)(whenFn: c.Expr[(P1, P2) => Boolean]): c.Expr[WithWhen2[P1, P2, R]] = {
    import c.universe._
    reify {
      val produces = (c.Expr[Produces2[P1, P2, R]](c.prefix.tree)).splice
      val string = c.literal(show(whenFn.tree)).splice
      WithWhen2[P1, P2, R](whenFn.splice, produces.data, string)(produces.a)
    }
  }
  def because_impl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag](c: blackbox.Context)(becauseFn: c.Expr[PartialFunction[(P1, P2), R]]): c.Expr[WithBecause[(P1, P2), R]] = {
    import c.universe._
    reify {
      val produces = (c.Expr[Produces2[P1, P2, R]](c.prefix.tree)).splice
      val string = c.literal(show(becauseFn.tree)).splice
      WithBecause[(P1, P2), R](becauseFn.splice, produces.data, CodeHolder.prettyDescription(string))(produces.a)
    }
  }
}

case class WithWhen2[P1, P2, R](whenFn: (P1, P2) => Boolean, data: ScenarioBuilderData[(P1, P2), R], ifString: String)(implicit a: ScenarioAggregator[(P1, P2), R]) extends ScenarioBuilderComponent[WithWhen2[P1, P2, R], (P1, P2), R] {
  override protected def rawCopyWith(fn: ScenarioBuilderData[(P1, P2), R] => ScenarioBuilderData[(P1, P2), R]): WithWhen2[P1, P2, R] = copy(data = data)
  override val scenarioReason: SingleScenarioLogic[(P1, P2), R] = SingleScenarioLogic(data.result, Some(whenFn.tupled), None, data.isDefinedAt, ifString)
}



