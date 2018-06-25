package one.xingyi.cddutilities

package object optics {
  type Validator[T, Issue] = List[String] => T => List[Issue]

}
