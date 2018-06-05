package one.xingyi.cddutilities

trait ShortPrint[T] extends (T => String)
object ShortPrint {
  implicit def default[T]: ShortPrint[T] = _.toString
}
