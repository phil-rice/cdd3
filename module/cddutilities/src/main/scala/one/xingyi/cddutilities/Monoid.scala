package one.xingyi.cddutilities


trait SemiGroup[T] {
  def add(t1: T, t2: T): T
}
object SemiGroup {
  implicit def forString: SemiGroup[String] = { (t1, t2) => t1 + t2 }
  implicit def forList[T]: SemiGroup[List[T]] = { (t1, t2) => t1 ++ t2 }
  implicit def forOption[T]: SemiGroup[Option[T]] = { (t1, t2) => t1.fold(t2)(_ => t1) }
  implicit def forPartialFn[P,R]: SemiGroup[PartialFunction[P,R]] = { (t1, t2) => t1 orElse t2}
}

trait SemiGroupLanguage {
  implicit class SemiGroupops[T](t: T)(implicit semiGroup: SemiGroup[T]) {
    def or(t1: T): T = semiGroup.add(t, t1)
  }
  implicit class SeqOfSemiGroups[T](ts: Seq[T])(implicit semiGroup: SemiGroup[T]) {
    def orAll = ts.reduce(semiGroup.add)
  }
}
object SemiGroupLanguage extends SemiGroupLanguage

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
trait MonoidLanguage {
  implicit class MonoidOps[T](t: T)(implicit monoid: Monoid[T]) {
    def |+|(t1: T): T = monoid.add(t, t1)
  }

}
object MonoidLanguage extends MonoidLanguage {
}
