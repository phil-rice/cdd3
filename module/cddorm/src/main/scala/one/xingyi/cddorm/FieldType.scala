package one.xingyi.cddorm

sealed trait FieldType[T] {
  def name: String
  def typeName: String
}
case class StringField(name: String, typeName: String = "varchar(255)") extends FieldType[String]
case class IntField(name: String, typeName: String = "integer") extends FieldType[Int]