/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddutilities.functions

trait SemiGroup[T] {
  def add(t1: T, t2: T): T
}
object SemiGroup {
  implicit def forString: SemiGroup[String] = { (t1, t2) => t1 + t2 }
  implicit def forList[T]: SemiGroup[List[T]] = { (t1, t2) => t1 ++ t2 }
  implicit def forOption[T]: SemiGroup[Option[T]] = { (t1, t2) => t1.fold(t2)(_ => t1) }
  implicit def forPartialFn[P,R]: SemiGroup[PartialFunction[P,R]] = { (t1, t2) => t1 orElse t2}
}

object SemiGroupLanguage extends SemiGroupLanguage
trait SemiGroupLanguage {
  implicit class SemiGroupops[T](t: T)(implicit semiGroup: SemiGroup[T]) {
    def |+|(t1: T): T = semiGroup.add(t, t1)
    def or(t1: T): T = semiGroup.add(t, t1)
  }
  implicit class SeqOfSemiGroups[T](ts: Seq[T])(implicit semiGroup: SemiGroup[T]) {
    def orAll = ts.reduce(semiGroup.add)
  }
}



trait Zero[T] {
  def zero: T
}
object Zero {
  implicit def forString: Zero[String] = new Zero[String] {def zero = ""}
  implicit def forList[T]: Zero[List[T]] = new Zero[List[T]] {def zero = List()}
}

trait Monoid[T] extends SemiGroup[T] with Zero[T]

object Monoid {
  implicit def monoidForZeroAndSemigroup[T](implicit semigroup: SemiGroup[T], z: Zero[T]): Monoid[T] = new Monoid[T] {
    override def add(t1: T, t2: T): T = semigroup.add(t1, t2)
    override def zero: T = z.zero
  }
}
trait MonoidLanguage extends SemiGroupLanguage{
//  implicit class MonoidOps[T](t: T)(implicit monoid: Monoid[T]) {
//  }

}
object MonoidLanguage extends MonoidLanguage {
}
