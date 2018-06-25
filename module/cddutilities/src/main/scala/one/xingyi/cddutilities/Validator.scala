package one.xingyi.cddutilities
import one.xingyi.cddutilities.optics.Validator


object Validators{
  def noValidator[T,Issue]: Validator[T,Issue] = _ => _ => List()
}
