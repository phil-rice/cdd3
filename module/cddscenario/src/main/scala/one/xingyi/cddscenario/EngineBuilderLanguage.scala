package one.xingyi.cddscenario

import one.xingyi.cddscenario.EngineComponentData._
import one.xingyi.cddutilities.{CodeHolder, DefinedInSourceCodeAt, IdMaker, Lens}

import scala.annotation.implicitNotFound
import scala.language.higherKinds
import scala.reflect.macros.blackbox

//trait CanHaveWhenOrBecause[T]
//
import one.xingyi.cddutilities.AnyLanguage._
sealed trait YesNo
trait Yes extends YesNo
trait No extends YesNo

class ScBuilder[P, R, HasResult, HasWhen, HasCode, HasBecause](protected[cddscenario] val id: Int,
                                                               protected[cddscenario] val situation: P,
                                                               protected[cddscenario] val data: EngineComponentData,
                                                               protected[cddscenario] val optResult: Option[R] = None,
                                                               protected[cddscenario] val optWhen: Option[P => Boolean] = None,
                                                               protected[cddscenario] val optCode: Option[P => R] = None,
                                                               protected[cddscenario] val optBecause: Option[PartialFunction[P, R]] = None)(implicit protected[cddscenario] val scenarioAggregator2: ScenarioAggregator2[P, R]) {
  scenarioAggregator2(this)
  protected[cddscenario] def withData(data: EngineComponentData) = new ScBuilder[P, R, HasResult, HasWhen, HasCode, HasBecause](id, situation, data, optResult, optWhen, optCode, optBecause)

  //TODO revisit this and sort out scenario logic. Also need the if string and perhaps the then string
  protected[cddscenario] def scenarioLogic = optBecause match {
    case None => SingleScenarioLogic(optResult, optWhen, optCode, data.definedInSourceCodeAt, "not yet for scbuidler")
    case Some(b) => SingleScenarioLogic(optResult, Some(b.isDefinedAt _), Some(b), data.definedInSourceCodeAt, "not yet for scbuilder")
  }
  def asScenario = Scenario[P, R](situation, scenarioLogic, List(), data)
}


object ScBuilder {
  implicit def toData[P, R, HasResult, HasWhen, HasCode, HasBecause](implicit scenarioAggregator2: ScenarioAggregator2[P, R]): Lens[ScBuilder[P, R, HasResult, HasWhen, HasCode, HasBecause], EngineComponentData] =
    Lens(_.data, (w, d) => w.withData(data = d))
}

@implicitNotFound("""If you are making the scenario using a use case you will have one of these. If you NEED to have no aggregator and know what you are doing, you can import the NullScenarioAggregator""")
trait ScenarioAggregator2[P, R] extends (ScBuilder[P, R, _, _, _, _] => Unit)

object NullScenarioAggregator2 {
  implicit def nullAggregator[P, R]: ScenarioAggregator2[P, R] = scenario => {}
}

class RememberingScenarioAggregator2[P, R] extends ScenarioAggregator2[P, R] {
  private var list = List[ScBuilder[P, R, _, _, _, _]]()
  private val lock = new Object()
  def withAggreator[X](fn: ScenarioAggregator2[P, R] => X): (X, List[Scenario[P, R]]) = (fn(this), scenarios)

  def scenarios = list.map(_.asScenario)
  override def apply(comp: ScBuilder[P, R, _, _, _, _]): Unit = list = list.filterNot(_.id == comp.id) :+ comp
}


trait EngineBuilderLanguage extends IdMaker {
  implicit class LensToEngineDataOps[T](t: T)(implicit toDataL: Lens[T, EngineComponentData]) {
    def comment(comment: String) = toDataL andThen toCommentL set(t, Some(comment))
    def title(title: String) = toDataL andThen toTitleL set(t, Some(title))
    def reference(doc: Document) = toDataL andThen toReferencesL transform(t, _ :+ Reference(doc))
  }
  def scenario[P, R](p: P)(implicit scenarioAggregator: ScenarioAggregator2[P, R]) = new ScBuilder[P, R, No, No, No, No](getNextId, p, EngineComponentData(DefinedInSourceCodeAt.definedInSourceCodeAt(), None))

  implicit class ScBuilderAddResultOps[P, R, HasWhen](scBuilder: ScBuilder[P, R, No, HasWhen, No, No]) {
    import scBuilder._
    def produces(r: R) = new ScBuilder[P, R, Yes, HasWhen, No, No](id, situation, data, Some(r), optWhen, optCode, optBecause)
  }

  implicit class ScBuilderAddWhenOps[P, R, HasResult, HasCode](val scBuilder: ScBuilder[P, R, HasResult, No, HasCode, No]) {
    import scBuilder._
    def when(when: P => Boolean) = new ScBuilder[P, R, HasResult, Yes, HasCode, No](id, situation, data, optResult, Some(when), optCode, optBecause)
  }

  // when { p:Int => p < 2 } be
  implicit val agg: ScenarioAggregator2[Int, String] = ???
  scenario(1) when (_ < 2) produces "someResult" title "sometitle" comment "somecomment"
}


object EngineBuilderLanguage {

//  object Produces {
//    def when_impl[P: c.WeakTypeTag, R: c.WeakTypeTag, HasResult: c.WeakTypeTag, HasCode: c.WeakTypeTag](c: blackbox.Context)(whenFn: c.Expr[P => Boolean]): c.Expr[ScBuilder[P, R, HasResult, Yes, HasCode, No]] = {
//      import c.universe._
//      reify {
//        val produces = (c.Expr[ScBuilderAddWhenOps[P, R]](c.prefix.tree)).splice
//        new ScBuilder[P, R, HasResult, Yes, HasCode, No](id, situation, data, optResult, Some(when), optCode, optBecause)
//        WithWhen[P, R](whenFn.splice, produces.data, c.literal(show(whenFn.tree)).splice)(produces.a)
//      }
//    }
//    def because_impl[P: c.WeakTypeTag, R: c.WeakTypeTag](c: blackbox.Context)(becauseFn: c.Expr[PartialFunction[P, R]]): c.Expr[WithBecause[P, R]] = {
//      import c.universe._
//      reify {
//        val produces = (c.Expr[Produces[P, R]](c.prefix.tree)).splice
//        val string = c.literal(show(becauseFn.tree)).splice
//        WithBecause[P, R](becauseFn.splice, produces.data, CodeHolder.prettyDescription(string))(produces.a)
//      }
//    }
//
//
//  }
}