package one.xingyi.cddutilities

case class IndentAnd[T](depth: Int, t: T)

object IndentAnd {
  implicit def shortPrint[T](implicit p: ShortPrint[T]): ShortPrint[IndentAnd[T]] = { t => Strings.blanks(t.depth) + p(t.t) }
}