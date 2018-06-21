package one.xingyi.cddorm
import java.sql.PreparedStatement

import scala.language.higherKinds

sealed trait FieldType[T] {
  def name: String
  def clazz: Class[_]
  def typeName: String
  def addToStatement(preparedStatement: PreparedStatement, i: Int, t: T)
}
case class StringField(name: String, typeName: String = "integer") extends FieldType[String] {
  override def addToStatement(ps: PreparedStatement, i: Int, s: String): Unit = ps.setString(i, s)
  override def clazz: Class[_] = classOf[String]
}
case class IntField(name: String, typeName: String = "varchar(255)") extends FieldType[Int] {
  override def addToStatement(ps: PreparedStatement, i: Int, t: Int): Unit = ps.setInt(i, t)
  override def clazz: Class[_] = classOf[Int]
}

case class ResultSetData(map: String => FieldType[_])

trait OrmEntity {
  def tableName: String
  def alias: String
  def primaryKeyField: FieldType[Int]

  def dataFields: List[FieldType[_]]
  def children: List[ChildEntity]
}

trait ChildEntity extends OrmEntity

case class MainEntity(tableName: String, alias: String, primaryKeyField: FieldType[Int], dataFields: List[FieldType[_]], children: List[ChildEntity]) extends OrmEntity
case class OneToManyEntity(tableName: String, alias: String, primaryKeyField: FieldType[Int], parentId: FieldType[Int], dataFields: List[FieldType[_]], children: List[ChildEntity]) extends ChildEntity


case class BatchDetails(batchSize: Int, index: Int) {
  def offset = batchSize * index
}
trait SqlOps {
  def dropTempTable(e: OrmEntity) = s"drop table if exists temp_${e.tableName}"
  def createMainTempTable(batchDetails: BatchDetails)(e: OrmEntity): String =
    s" create temporary table temp_${e.tableName} as select ${selectFields(e)} from ${e.tableName} limit ${batchDetails.batchSize} offset ${batchDetails.offset}"

  def createChildTempTable(parent: OrmEntity)(e: OneToManyEntity): String =
    s" create temporary table temp_${e.tableName} as select ${selectFields(e)} from ${e.tableName} where ${parent.primaryKeyField.name} = ${e.parentId.name}"

  def drainSql(e: OrmEntity): String = s"select * from ${e.tableName}"

  def selectFields(e: OrmEntity) = (e.primaryKeyField :: e.dataFields).map(_.name).mkString(",")
}

object SqlOps{
  implicit object DefaultSqlOps extends SqlOps
}

object OrmStrategies extends OrmStrategies
trait OrmStrategies {
  def dropTempTables(implicit sqlOps: SqlOps): EntityStrategy[String] = EntityStrategy(sqlOps.dropTempTable, _ => sqlOps.dropTempTable)
  def createTempTables(batchDetails: BatchDetails)(implicit sqlOps: SqlOps): EntityStrategy[String] = EntityStrategy(sqlOps.createMainTempTable(batchDetails), sqlOps.createChildTempTable)
  def drainTempTables(implicit sqlOps: SqlOps): EntityStrategy[String] = EntityStrategy(sqlOps.drainSql, _ => sqlOps.drainSql)
}

case class EntityStrategy[X](mainEntity: OrmEntity => X, oneToManyEntity: OrmEntity => OneToManyEntity => X) {
  def childEntity(parentEntity: OrmEntity): PartialFunction[ChildEntity, X] = {case e: OneToManyEntity => oneToManyEntity(parentEntity)(e)}
  def walk(e: MainEntity): List[(OrmEntity, Any)] = (e, mainEntity(e)) :: e.children.flatMap(walkChildren(e))
  def walkChildren(parent: OrmEntity)(child: ChildEntity): List[(OrmEntity, X)] = (child, childEntity(parent)(child)) :: child.children.flatMap(walkChildren(child))
}


