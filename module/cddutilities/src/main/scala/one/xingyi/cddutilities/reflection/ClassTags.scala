package one.xingyi.cddutilities.reflection
import scala.reflect.ClassTag

class ClassTags {
  def nameOf[A](implicit classTag: ClassTag[A]) = classTag.runtimeClass.getSimpleName
  def mustBeSame[Expected, Actual](implicit classTaga: ClassTag[Expected], classTagb: ClassTag[Actual])=
    if (classTaga.runtimeClass != classTagb.runtimeClass) throw new RuntimeException(s"Class mimatch. Expected ${nameOf[Expected]} Actual ${nameOf[Actual]}")

}
