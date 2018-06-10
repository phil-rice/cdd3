package one.xingyi.cddutilities


object AnyLanguage extends AnyLanguage
trait AnyLanguage {
  implicit class AnyOps[T](t: T) {
    def ifError(fn: Exception => T): T = try {
      t
    } catch {
      case e: Exception => fn(e)
    }
  }

  def using[T, T1](t: T)(fn: T => T1) = fn(t)

  implicit class BooleanOps(b: Boolean) {
    def asOption[T](t: => T) = if (b) Some(t) else None
  }
}

object FunctionLanguage extends FunctionLanguage
trait FunctionLanguage {
  implicit class FunctionToOptionOps[From, To](fn: From => Option[To]) {
    def orElse[T](fn2: From => Option[To]): From => Option[To] = { from: From => fn2(from) }
    def orDefault(to: => To): From => To = { from: From => fn(from).getOrElse(to) }
  }
  implicit class FunctionFromMidToOptionOps[From, Mid, To](fn: From => Mid => Option[To]) {
    def orElse[T](fn2: From => Mid => Option[To]): From => Mid => Option[To] = { from: From => mid: Mid => fn2(from)(mid) }
    def orDefault(to: => To): From => Mid => To = { from: From => mid: Mid => fn(from)(mid).getOrElse(to) }
  }
}

trait Arrows {
  case class use[P, X](thingMaker: P => X) {
    def apply[R](fn: X => R): P => R = { p => fn(thingMaker(p)) }
  }
  case class use2[P1, P2, X](thingMaker: (P1, P2) => X) {
    def apply[R](fn: X => PartialFunction[(P1, P2), R]) = { (p1: P1, p2: P2) => fn(thingMaker(p1, p2))(p1, p2) }
  }

  implicit class FunctionPimper[P, R](fn: P => R) {
    def sideeffect[X](block: R => X): P => R = { p: P => val result = fn(p); block(result); result }
  }


}
object Arrows extends Arrows
import Arrows._
