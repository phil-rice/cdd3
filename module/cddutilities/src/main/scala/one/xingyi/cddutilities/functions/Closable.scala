package one.xingyi.cddutilities.functions

import scala.language.higherKinds

trait ClosableM[M[_]] {
  def liftM[T](t: T, closables: Seq[AutoCloseable]): M[T]
  def map[T, T1](m: M[T], fn: T => T1): M[T1]
  def flatmap[T, T1](m: M[T], fn: T => M[T1]): M[T1]
  def join[T, T1](values: Seq[M[T1]]): M[Seq[T1]]
  def close[T](m: M[T]): T
}

object ClosableLanguage extends ClosableLanguage
trait ClosableLanguage {

  implicit class ClosableAnyOps[T](t: T) {
    def liftM[M[_]](closables: AutoCloseable*)(implicit closable: ClosableM[M]) = closable.liftM(t, closables)
  }
  implicit class CloseableOps[T <: AutoCloseable](t: T) {
    def liftClosable[M[_]](implicit closable: ClosableM[M]) = closable.liftM(t, Seq(t))
  }
  implicit class ClosableOps[M[_], T](m: M[T])(implicit closable: ClosableM[M]) {
    def map[T1](fn: T => T1) = closable.map(m, fn)
    def flatmap[T1](fn: T => M[T1]) = closable.flatmap(m, fn)
    def close() = closable.close(m)
  }

  implicit class ClosableKleisliOps[M[_] : ClosableM, From, To](k1: From => M[To]) {
    def ==>[T](k2: To => T): From => M[T] = { from: From => k1(from).map(k2) }
    def ===>[T](k2: To => M[T]): From => M[T] = { from: From => k1(from).flatmap(k2) }
    def =+=>[T](k2: To => To => M[T]) = { from: From => k1(from).flatmap(to => k2(to)(to)) }
    def =>=>[T](k2: To => M[T]): From => M[(To, T)] = { from: From => k1(from).flatmap(to => k2(to).map(t => (to, t))) }
    //    def =>=>[T](k2: () => M[T]): From => M[(To, T)] = { from: From => k1(from).flatmap(to => k2().map(t => (to, t))) }
    def ====>[T](k2: To => T): From => T = { from: From => k2(k1(from).close) }
  }

  implicit class ClosableKleislTupleOps[M[_] : ClosableM, From, T1, T2](fn: From => M[(T1, T2)]) {
    def -=>[T](k2: T1 => T): From => M[(T, T2)] = { from: From => fn(from).map { tuple => (k2(tuple._1), tuple._2) } }
    def -==>[T](k2: T1 => M[T]) = { from: From => fn(from).flatmap { tuple => k2(tuple._1).map(x => (x, tuple._2)) } }

  }

  def result[T] = { t: T => t }

  def join[M[_], From, T](fns: (From => M[T])*)(implicit closableM: ClosableM[M]): From => M[Seq[T]] = { from: From =>
    closableM.join(fns.map(fn => fn(from)))

  }
  def inParallel[M[_] : ClosableM, From, T1](fn1: From => M[T1]) = new InParallelWord[M, From, T1](fn1)
  class InParallelWord[M[_], From, T1](fn1: From => M[T1])(implicit closableM: ClosableM[M]) {
    def and[T2](fn2: From => M[T2]) = new AndWord(fn2)
    class AndWord[T2](fn2: From => M[T2]) {
      def merge[Result](merge: T1 => T2 => Result): From => M[Result] = { from: From =>
        val mt1 = fn1(from)
        val mt2 = fn2(from)
        mt1.flatmap(t1 => mt2.map(t2 => merge(t1)(t2)))
      }
    }
  }
}

case class SimpleClosable[T](value: T, closables: Seq[AutoCloseable]) {
  def addClosersFrom[TOld](m: SimpleClosable[TOld]) = SimpleClosable(value, closables ++ m.closables )
}
object SimpleClosable {
  implicit object SimpleClosableMonad extends ClosableM[SimpleClosable] {
    override def liftM[T](t: T, closables: Seq[AutoCloseable]): SimpleClosable[T] = SimpleClosable(t, closables)
    override def map[T, T1](m: SimpleClosable[T], fn: T => T1): SimpleClosable[T1] = SimpleClosable(fn(m.value), m.closables)
    override def flatmap[T, T1](m: SimpleClosable[T], fn: T => SimpleClosable[T1]) = fn(m.value) addClosersFrom m
    override def close[T](m: SimpleClosable[T]): T = {m.closables.foreach{c=>c.close}; m.value}

    override def join[T, T1](values: Seq[SimpleClosable[T1]]): SimpleClosable[Seq[T1]] = {
      val vs = values.map(_.value)
      val closables = values.foldLeft(List[AutoCloseable]())((acc, c) => acc ++ c.closables)
      SimpleClosable(vs, closables)
    }
  }
}
