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
