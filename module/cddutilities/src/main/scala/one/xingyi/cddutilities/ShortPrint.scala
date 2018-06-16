package one.xingyi.cddutilities

trait ShortPrint[T] extends (T => String)
object ShortPrint {
  implicit def default[T]: ShortPrint[T] = _.toString
  def apply[T](t:T)(implicit shortPrint: ShortPrint[T])= shortPrint(t)
}
