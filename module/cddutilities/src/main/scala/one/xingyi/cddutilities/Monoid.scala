package one.xingyi.cddutilities

trait Semigroup[T] {
  def add(t1: T, t2: T): T
}
object Semigroup {
  implicit def forString: Semigroup[String] = { (t1, t2) => t1 + t2 }
  implicit def forList[T]: Semigroup[List[T]] = { (t1, t2) => t1 ++ t2 }
}

trait Zero[T] {
  def zero: T
}
object Zero {
  implicit def forString: Zero[String] = new Zero[String] {def zero = ""}
  implicit def forList[T]: Zero[List[T]] = new Zero[List[T]] {def zero = List()}
}

trait Monoid[T] extends Semigroup[T] with Zero[T]

object Monoid {
  implicit def monoidForZeroAndSemigroup[T](implicit semigroup: Semigroup[T], z: Zero[T]): Monoid[T] = new Monoid[T] {
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
