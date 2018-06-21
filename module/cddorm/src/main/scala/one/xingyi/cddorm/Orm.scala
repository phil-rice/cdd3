package one.xingyi.cddorm
import scala.language.higherKinds

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
  def dropTable(e: OrmEntity) = s"drop table if exists ${e.tableName}"
  def dropTempTable(e: OrmEntity) = s"drop table if exists ${tempTableName(e)}"
  def tempTableName(e: OrmEntity) = "temp_" + e.tableName
  def createTable(e: OrmEntity): String = {
    s"create table ${e.tableName} (${(e.primaryKeyField :: e.dataFields).map(f => f.name + " " + f.typeName).mkString(", ")})"
  }
  def createOneToManyTable(e: OneToManyEntity): String = {
    s"create table ${e.tableName} (${(e.primaryKeyField :: e.parentId :: e.dataFields).map(f => f.name + " " + f.typeName).mkString(", ")})"
  }

  def createMainTempTable(batchDetails: BatchDetails)(e: OrmEntity): String =
    s"create temporary table ${tempTableName(e)} as select ${selectFields(e)} from ${e.tableName} ${e.alias} limit ${batchDetails.batchSize} offset ${batchDetails.offset}"

  def createChildTempTable(parent: OrmEntity)(e: OneToManyEntity): String =
    s"create temporary table temp_${e.tableName} as select ${e.alias}.${e.parentId.name}, ${selectFields(e)} from ${tempTableName(parent)} ${parent.alias},${e.tableName} ${e.alias}  where ${parent.alias}.${parent.primaryKeyField.name} = ${e.alias}.${e.parentId.name}"

  def drainSql(e: OrmEntity): String = s"select * from ${tempTableName(e)}"

  def selectFields(e: OrmEntity) = (e.primaryKeyField :: e.dataFields).map(f => e.alias + "." + f.name).mkString(", ")
}

object SqlOps {
  implicit object DefaultSqlOps extends SqlOps
}

object OrmStrategies extends OrmStrategies
trait OrmStrategies {
  def dropTables(implicit sqlOps: SqlOps): EntityStrategy[String] = EntityStrategy(sqlOps.dropTable, _ => sqlOps.dropTable)
  def createTables(implicit sqlOps: SqlOps): EntityStrategy[String] = EntityStrategy(sqlOps.createTable, _ => sqlOps.createOneToManyTable)

  def dropTempTables(implicit sqlOps: SqlOps): EntityStrategy[String] = EntityStrategy(sqlOps.dropTempTable, _ => sqlOps.dropTempTable)
  def createTempTables(batchDetails: BatchDetails)(implicit sqlOps: SqlOps): EntityStrategy[String] = EntityStrategy(sqlOps.createMainTempTable(batchDetails), sqlOps.createChildTempTable)
  def drainTempTables(implicit sqlOps: SqlOps): EntityStrategy[String] = EntityStrategy(sqlOps.drainSql, _ => sqlOps.drainSql)
}

case class EntityStrategy[X](mainEntity: OrmEntity => X, oneToManyEntity: OrmEntity => OneToManyEntity => X) {
  def map[T](fn: X => T) = EntityStrategy(mainEntity andThen fn, p => c => fn(oneToManyEntity(p)(c)))
//  def map2[T](fn: OrmEntity => X => T) = EntityStrategy(mainEntity andThen {x:X => fn(mainEntity)(x)}, p => c => fn(c)(oneToManyEntity(p)(c)))
  def childEntity(parentEntity: OrmEntity): PartialFunction[ChildEntity, X] = {case e: OneToManyEntity => oneToManyEntity(parentEntity)(e)}
  def walk(e: MainEntity): List[(OrmEntity, X)] = (e, mainEntity(e)) :: e.children.flatMap(walkChildren(e))
  def walkChildren(parent: OrmEntity)(child: ChildEntity): List[(OrmEntity, X)] = (child, childEntity(parent)(child)) :: child.children.flatMap(walkChildren(child))
}


