package one.xingyi.cddutilities


trait Arrows {
  case class use[P, X](thingMaker: P => X) {
    def apply[R](fn: X => R): P => R = { p => fn(thingMaker(p)) }
  }
  case class use2[P1, P2, X](thingMaker: (P1, P2) => X) {
    def apply[R](fn: X => PartialFunction[(P1, P2), R]) = { (p1: P1, p2: P2) => fn(thingMaker(p1, p2))(p1, p2) }
  }

}
object Arrows extends Arrows
import Arrows._
