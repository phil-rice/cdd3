/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddutilities.language

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
//    def sideeffectTry(fn: Try[T] => Unit): Try[T] = {
//      val triedT = Try(t)
//      fn(triedT)
//      triedT
//    }
  }

  def using[T, T1](t: T)(fn: T => T1) = fn(t)

  implicit class BooleanOps(b: Boolean) {
    def asOption[T](t: => T) = if (b) Some(t) else None
  }
}

object FunctionLanguage extends FunctionLanguage
trait FunctionLanguage {

//  implicit class FunctionOps[From, To](fn1: From => To) {
//    def andThenWithFrom[T](fn2: From => To => T) = { from: From => fn2(from)(fn1(from)) }
//    def wrap(startFn: From => Unit)(endFn: Try[To] => Unit): From => Try[To] = { from: From =>
//      startFn(from)
//      val result = Try(fn1(from))
//      endFn(result)
//      result
//    }
//  }
//  implicit class FunctionToOptionOps[From, To](fn: From => Option[To]) {
//    def orElse[T](fn2: From => Option[To]): From => Option[To] = { from: From => fn2(from) }
//    def orDefault(to: => To): From => To = { from: From => fn(from).getOrElse(to) }
//  }
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
