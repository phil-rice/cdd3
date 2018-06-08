package one.xingyi.cddutilities

trait Functions {
  implicit class OptionalFunctionOps[P, R](opt: Option[P => R]) {
    def applyOr(p: P, default: P => R): R = opt.fold(default(p))(_.apply(p))
    def asPFn(defined: Option[P => Boolean]) = opt.map(functionPartialFunction[P, R](defined))
  }
  private def functionPartialFunction[P, R](defined: Option[P => Boolean])(fn: P => R): PartialFunction[P, R] = {case p if defined.map(_.apply(p)).getOrElse(true) => fn(p)}
  private def constantPartialFunction[P, R](defined: Option[P => Boolean])(r: R): PartialFunction[P, R] = {case p if defined.map(_.apply(p)).getOrElse(true) => r}

  implicit class OptionFunctionOps[T](opt: Option[T]) {
    def asFun[P]: Option[P => T] = opt.map(r => { p: P => r })
    def asConstantPFn[P](defined: Option[P => Boolean]): Option[PartialFunction[P, T]] = opt.map(constantPartialFunction(defined))
  }

  //  implicit class FunctionOps[P, R](fn: P => R) {
  //    def asPf(defined: P => Boolean): PartialFunction[P, R] = {case p if defined(p) => fn(p)}
  //  }

}

trait PartialFunctions extends Functions {
  implicit class OptionalPartialFunctionOps[P, R](opt: Option[PartialFunction[P, R]]) {
    def isDefinedAt(p: P, default: => Boolean): Boolean = opt.fold(default)(_.isDefinedAt(p))
  }
}

object PartialFunctions extends PartialFunctions