/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddutilities.language

import java.util.concurrent.atomic.AtomicInteger

import one.xingyi.cddutilities.functions.ClosableFunction1

import scala.util.Try
object AnyLanguage extends AnyLanguage
trait AnyLanguage {
  implicit class AnyOps[T](t: => T) {
    //    def ifError(fn: Exception => T): T = try {
    //      t
    //    } catch {
    //      case e: Exception => fn(e)
    //    }
    def sideeffect(fn: T => Unit): T = {
      val result = t
      fn(t)
      result
    }
    def sideeffectTry(fn: Try[T] => Unit): Try[T] = {
      val triedT = Try(t)
      fn(triedT)
      triedT
    }

    def applyTo[To](fn: T => To) = fn(t)
  }

  def using[T, T1](t: T)(fn: T => T1) = fn(t)

  implicit class BooleanOps(b: Boolean) {
    def asOption[T](t: => T) = if (b) Some(t) else None
  }

  implicit class AtomicIntegerOps(a: AtomicInteger) {
    def tick(size: Int)(fn: => Unit): Unit = {
      if (a.updateAndGet { old => if (old >= size - 1) 0 else old + 1 } == 0) fn
    }
    def ifNotZero(fn: => Unit): Unit = {
      if (a.get != 0) fn
    }
  }

  implicit class ListOps[X](s: List[X]){
    def asString(fn: X => String, separator: String = ",") = s.map(fn).mkString(separator)
  }
}


object Closer {
  implicit def tupleCloser[T1, T2](implicit closer1: Closer[T1], closer2: Closer[T2]): Closer[(T1, T2)] = tuple => try closer2(tuple._2) finally closer1(tuple._1)
}
trait Closer[T] extends (T => Unit)
object FunctionLanguage extends FunctionLanguage

class FromWord[From] {
  def make[To](fn: From => To)(implicit closer: Closer[From]) = new ClosableFunction1[From, To](fn)
  //  case class make[Mid](makeFn: From => Mid)(implicit midCloser: Closer[Mid]) {
  //    case class makeBoth[Mid2](makeFn2: Mid => Mid2)(implicit mid2Closer: Closer[Mid2]) {
  //      case class and[Mid3](makeFn3: Mid => Mid3)(implicit mid3Closer: Closer[Mid3]) {
  //        def thenDo[To](fn: Mid2 => Mid3 => To): From => To = { from: From =>
  //          val mid = makeFn(from)
  //          try {
  //            val mid2 = makeFn2(mid)
  //            try {
  //              val mid3 = makeFn3(mid)
  //              try {
  //                fn(mid2)(mid3)
  //              } finally {mid3Closer(mid3)}
  //            } finally {mid2Closer(mid2)}
  //          } finally {midCloser(mid)}
  //        }
  //      }
  //    }
  //    case class andMake[Mid2](makeFn2: Mid => Mid2)(implicit mid2Closer: Closer[Mid2]) {
  //      def thenDo[To](fn: Mid2 => To): From => To = { from: From =>
  //        val mid = makeFn(from)
  //        try {
  //          val mid2 = makeFn2(mid)
  //          try {fn(mid2)} finally {mid2Closer(mid2)}
  //        } finally {midCloser(mid)}
  //      }
  //    }
  //    def thenDo[To](fn: Mid => To)(implicit midCloser: Closer[Mid]): From => To = { from: From =>
  //      val mid = makeFn(from)
  //      try {fn(mid)} finally midCloser(mid)
  //    }

}
trait FunctionLanguage {

  def from[From] = new FromWord[From]
  implicit class FunctionOps[From, To](fn1: From => To) {
    //    def callThenClose[RealFrom](startFn: RealFrom => From)(closeFn: Try[To] => Unit): RealFrom => To = { realFrom: RealFrom =>
    //      startFn(realFrom)
    //      val result = Try(fn1(realFrom))
    //      closeFn(result)
    //      result.get
    //    }
  }
  implicit class FunctionToOptionOps[From, To](fn: From => Option[To]) {
    //    def orElse[T](fn2: From => Option[To]): From => Option[To] = { from: From => fn2(from) }
    //    def orDefault(to: => To): From => To = { from: From => fn(from).getOrElse(to) }
  }
  implicit class FunctionFromMidToOptionOps[From, Mid, To](fn: From => Mid => Option[To]) {
    def orElse(fn2: From => Mid => Option[To]): From => Mid => Option[To] = { from: From => mid: Mid => fn(from)(mid).orElse(fn2(from)(mid)) }
    def orDefault(to: => To): From => Mid => To = { from: From => mid: Mid => fn(from)(mid).getOrElse(to) }
  }

}

trait Arrows {
  case class use[P, X](thingMaker: P => X) {
    def apply[R](fn: X => R): P => R = { p => fn(thingMaker(p)) }
  }
  //  case class use2[P1, P2, X](thingMaker: (P1, P2) => X) {
  //    def apply[R](fn: X => PartialFunction[(P1, P2), R]) = { (p1: P1, p2: P2) => fn(thingMaker(p1, p2))(p1, p2) }
  //  }

  //  implicit class FunctionPimper[P, R](fn: P => R) {
  //    def sideeffect[X](block: R => X): P => R = { p: P => val result = fn(p); block(result); result }
  //  }


}
object Arrows extends Arrows
