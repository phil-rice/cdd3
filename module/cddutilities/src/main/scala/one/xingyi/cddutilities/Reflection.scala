/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddutilities

import java.lang.annotation.Annotation
import java.lang.reflect.{Field, InvocationTargetException, Method}

import scala.collection.immutable.ListMap
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

object Reflection {

  implicit def toFieldMapPimper[V](fieldMap: ListMap[Field, Try[V]]) = new FieldMapPimper(fieldMap)

  def apply(instance: Any) = new Reflection(instance)

  def getAllFields(clazz: Class[_]): List[Field] =
    clazz.getDeclaredFields.toList ::: (clazz.getSuperclass match {
      case null => List();
      case c => getAllFields(c)
    })

  def getAllMethods(clazz: Class[_]): List[Method] =
    clazz.getDeclaredMethods.toList ::: (clazz.getSuperclass match {
      case null => List();
      case c => getAllMethods(c)
    })


  def instantiate[T](clazz: Class[T]): T = {
    import scala.reflect.runtime.{universe => ru}
    val rm = ru.runtimeMirror(clazz.getClassLoader())
    val declaredFields = clazz.getFields().toList
    val moduleField = declaredFields.find(field => field.getName() == "MODULE$")
    try {
      val obj = moduleField match {
        case Some(modField) => modField.get(clazz)
        case None => clazz.newInstance()
      }
      obj.asInstanceOf[T]
    } catch {
      case e: Throwable => throw new RuntimeException(s"Instantiating class: $clazz moduleField (i.e. 'is an object not a class'): $moduleField", e)
    }
  }

}

class FieldMapPimper[V](fieldMap: ListMap[Field, Try[V]]) {
  def sorted(allFields: List[Field]) = ListMap[Field, Try[V]](fieldMap.toList.sortBy { case (f, _) => allFields.indexOf(f) }: _*)

  def displayStringMap(valueFn: V => String = (v: V) => Strings.oneLine(v)): ListMap[Field, Try[String]] = {
    fieldMap.map { case (f, tryV) =>
      val s = tryV.transform(x => Try(valueFn(x)), e => Success(s"<Error>${e.getClass.getSimpleName}/${e.getMessage}</error>"))
      (f, s)
    }
  }

  def displayString(separator: String = "\r\n") = fieldMap.toList.map{case (f, v) => f.getName +" -> " +v.get}.mkString(separator) //yes this throws the exception if v has one. if you don't want that, don't have an exception here

  def removeAnnotationFromFieldMap[A <: Annotation : ClassTag]: ListMap[Field, Try[V]] = {
    val aClass = implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]]
    fieldMap.filter { case (f, tryV) => f.getAnnotation(aClass) == null }
  }
}


class Reflection(instance: Any) {

  import Reflection._

  //  val instanceClass = instance.getClass
  lazy val allFields = getAllFields(instance.getClass)
  lazy val allMethods = getAllMethods(instance.getClass)

  def instantiateIfLazyVal(field: Field) = {
    val methods = allMethods.
      filter(m => (m.getParameterTypes().length == 0) && field.getType == m.getReturnType && field.getName == m.getName)
    for (m <- methods) {
      m.setAccessible(true)
      m.invoke(instance)
    }
  }

  def getField[T: ClassTag](fieldName: String): Field = {
    val clazz = instance.getClass
    import scala.reflect.runtime.{universe => ru}
    val rm = ru.runtimeMirror(clazz.getClassLoader())

    val field = allFields.filter(_.getName == fieldName) match {
      case f :: _ => f
      case Nil => throw new NoSuchFieldException(s"Class is ${clazz} asked for field $fieldName, legal values are [${clazz.getDeclaredFields.mkString(", ")}]")
    }
    val askedForClass = implicitly[ClassTag[T]].runtimeClass
    if (askedForClass != field.getType) throw new scala.ClassCastException(s"Actual class ${field.getType}, asked for is ${askedForClass}")
    field.setAccessible(true) //A fairie may die every time this line is executed
    field
  }


  def getFieldValue[T: ClassTag](fieldName: String): Try[T] = {
    val field: Field = getField(fieldName)
    getFieldValue(field)
  }

  def getFieldValue[T](field: Field): Try[T] = try {
    instantiateIfLazyVal(field)
    field.setAccessible(true)
    Success(field.get(instance).asInstanceOf[T])
  } catch {
    case e: InvocationTargetException => Failure(e.getCause)
    case e: Exception => Failure(e)
  }

  def modField[T: ClassTag](fieldName: String)(fn: T => T) = {
    val field = getField[T](fieldName)
    val oldValue = field.get(instance).asInstanceOf[T]
    val newValue = fn(oldValue)
    field.set(instance, newValue)
  }

  def fieldMap[T: ClassTag]: ListMap[Field, Try[T]] = {
    val returnType = implicitly[ClassTag[T]].runtimeClass
    ListMap[Field, Try[T]]() ++ allFields.
      filter(f => returnType.isAssignableFrom(f.getType)).
      map((f) => (f -> getFieldValue[T](f)))
  }


  def fieldMapForAnnotation[A <: Annotation : ClassTag]: ListMap[Field, Try[Any]] = {
    val annotationType: Class[A] = implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]]
    ListMap[Field, Try[Any]]() ++ allFields.
      filter(f => f.getAnnotation(annotationType) != null).
      map((f) => (f -> getFieldValue[Any](f)))
  }

  //  def fieldMapToString[V](fieldMap: Map[Field, Try[V]], valueFn: V => String = (v: V) => Strings.oneLine(v), separator: String = "\r\n") =
  //    ListMap[Field, Try[V]](fieldMap.toList.sortBy { case (field, _) => allFields.indexOf(field) }: _*).map {
  //      case (f, v) =>
  //        val s = v.transform(x => Try(valueFn(x)), e => Success(s"<Error>${e.getClass.getSimpleName}/${e.getMessage}</error>")).get
  //        (f.getName, s)
  //    }.mkString(separator)
  //
  //  def removeAnnotationFromFieldMap[A <: Annotation : ClassTag](fieldMap: Map[Field, X])


}
