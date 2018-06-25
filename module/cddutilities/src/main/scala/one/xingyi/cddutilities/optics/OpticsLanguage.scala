package one.xingyi.cddutilities.optics

trait OpticsLanguage {
  implicit class GetterOps[A, B](fn1: A => B) {
    def |+|(setter: (A, B) => A) = Lens(fn1, setter)
  }
}
