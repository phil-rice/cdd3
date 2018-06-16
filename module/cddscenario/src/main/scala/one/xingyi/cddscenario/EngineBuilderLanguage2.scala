package one.xingyi.cddscenario
import one.xingyi.cddutilities.DefinedInSourceCodeAt

import scala.language.experimental.macros
import scala.language.higherKinds
import scala.reflect.macros.blackbox

object EngineBuilderLanguage2 {

  def when_impl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag, HasResult: c.WeakTypeTag, HasCode: c.WeakTypeTag](c: blackbox.Context)(when: c.Expr[(P1, P2) => Boolean]): c.Expr[ScBuilder[(P1, P2), R, HasResult, Yes, HasCode, No]] = {
    import c.universe._
    reify {
      val hasBuilder = (c.Expr[HasScBuilder[(P1, P2), R, HasResult, No, HasCode, No]](c.prefix.tree)).splice
      EngineBuilderLanguage.withWhenPrim(hasBuilder.builder, when.splice.tupled, c.literal(show(when.tree)).splice)
    }
  }
  def because_impl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag, HasResult: c.WeakTypeTag](c: blackbox.Context)(becauseFn: c.Expr[PartialFunction[(P1, P2), R]]): c.Expr[ScBuilder[(P1, P2), R, HasResult, No, No, Yes]] = {
    import c.universe._
    reify {
      val hasBuilder = (c.Expr[HasScBuilder[(P1, P2), R, HasResult, No, No, No]](c.prefix.tree)).splice
      EngineBuilderLanguage.withBecausePrim(hasBuilder.builder, becauseFn.splice, c.literal(show(becauseFn.tree)).splice, c.literal(show(becauseFn.tree)).splice)

    }
  }
}
trait EngineBuilderLanguage2 extends EngineBuilderLanguage {
  protected def scenario[P1, P2, R](p1: P1, p2: P2)(implicit scenarioAggregator: ScenarioAggregator2[(P1, P2), R]) =
    new ScBuilder[(P1, P2), R, No, No, No, No](getNextId, (p1, p2), EngineComponentData(DefinedInSourceCodeAt.definedInSourceCodeAt(), None), None, None, None, None, "", "")

  implicit protected class ScBuilderAddResultOps[P1, P2, R, HasWhen](val builder: ScBuilder[(P1, P2), R, No, HasWhen, No, No]) extends HasScBuilder[(P1, P2), R, No, HasWhen, No, No] {
    def produces(r: R): ScBuilder[(P1, P2), R, Yes, HasWhen, No, No] = builder.withResultPrim(r)
  }
  implicit protected class ScBuilderAddWhenOps[P1, P2, R, HasResult, HasCode](val builder: ScBuilder[(P1, P2), R, HasResult, No, HasCode, No]) extends HasScBuilder[(P1, P2), R, HasResult, No, HasCode, No] {
    def when(when: (P1, P2) => Boolean): ScBuilder[(P1,P2), R, HasResult, Yes, HasCode, No] = macro EngineBuilderLanguage2.when_impl[P1, P2, R, HasResult, HasCode]
  }
  implicit protected class ScBuilderAddBecauseOps[P1, P2, R, HasResult](val builder: ScBuilder[(P1, P2), R, HasResult, No, No, No]) extends HasScBuilder[(P1, P2), R, HasResult, No, No, No] {
    def because(becauseFn: PartialFunction[(P1, P2), R]): ScBuilder[(P1, P2), R, HasResult, No, No, Yes] = macro EngineBuilderLanguage1.because_impl[(P1, P2), R, HasResult]
  }
}