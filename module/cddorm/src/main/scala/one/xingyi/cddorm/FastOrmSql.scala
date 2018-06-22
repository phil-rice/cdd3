package one.xingyi.cddorm


/** This applies the sql defined in FastOrmSql to the entities in a composite entity  */
object OrmStrategies extends OrmStrategies
trait OrmStrategies {
  def dropTables(implicit fastOrmSql: FastOrmSql): EntityStrategy[String] = EntityStrategy(fastOrmSql.dropTable, _ => fastOrmSql.dropTable)
  def createTables(implicit fastOrmSql: FastOrmSql): EntityStrategy[String] = EntityStrategy(fastOrmSql.createTable, _ => fastOrmSql.createOneToManyTable)

  def dropTempTables(implicit fastOrmSql: FastOrmSql): EntityStrategy[String] = EntityStrategy(fastOrmSql.dropTempTable, _ => fastOrmSql.dropTempTable)
  def createTempTables(batchDetails: BatchDetails)(implicit fastOrmSql: FastOrmSql): EntityStrategy[String] = EntityStrategy(fastOrmSql.createMainTempTable(batchDetails), fastOrmSql.createChildTempTable)
  def drainTempTables(implicit fastOrmSql: FastOrmSql): EntityStrategy[String] = EntityStrategy(fastOrmSql.drainSql, _ => fastOrmSql.drainSql)
}

case class EntityStrategy[X](mainEntity: OrmEntity => X, oneToManyEntity: OrmEntity => OneToManyEntity => X) {
  def map[T](fn: X => T) = EntityStrategy(mainEntity andThen fn, p => c => fn(oneToManyEntity(p)(c)))
  def childEntity(parentEntity: OrmEntity): PartialFunction[ChildEntity, X] = {case e: OneToManyEntity => oneToManyEntity(parentEntity)(e)}
  def walk(e: MainEntity): List[(OrmEntity, X)] = (e, mainEntity(e)) :: e.children.flatMap(walkChildren(e))
  def walkChildren(parent: OrmEntity)(child: ChildEntity): List[(OrmEntity, X)] = (child, childEntity(parent)(child)) :: child.children.flatMap(walkChildren(child))
}

/** This is the layer of abstraction that needs to be rewritten for different databases. It's just a block of sql for each operation */
trait FastOrmSql {
  import FieldType.nameAndTypeName
  import one.xingyi.cddutilities.language.AnyLanguage._

  def dropTable(e: OrmEntity) = s"drop table if exists ${e.tableName}"
  def dropTempTable(e: OrmEntity) = s"drop table if exists ${tempTableName(e)}"
  def tempTableName(e: OrmEntity): String = "temp_" + e.tableName

  def createTable(e: OrmEntity): String = s"create table ${e.tableName} (${e.fieldsForCreate.asString(nameAndTypeName)})"
  def createOneToManyTable(e: OneToManyEntity): String = s"create table ${e.tableName} (${e.fieldsForCreate.asString(nameAndTypeName)})"

  def createMainTempTable(batchDetails: BatchDetails)(e: OrmEntity): String =
    s"create temporary table ${tempTableName(e)} as select ${selectFields(e)} from ${e.tableName} ${e.alias} limit ${batchDetails.batchSize} offset ${batchDetails.offset}"

  def createChildTempTable(parent: OrmEntity)(e: OneToManyEntity): String =
    s"create temporary table temp_${e.tableName} as select ${e.alias}.${e.parentId.name}, ${selectFields(e)} " +
      s"from ${tempTableName(parent)} ${parent.alias},${e.tableName} ${e.alias} " +
      s"where ${parent.alias}.${parent.primaryKeyField.name} = ${e.alias}.${e.parentId.name}"

  def drainSql(e: OrmEntity): String = s"select * from ${tempTableName(e)}"

  def selectFields(e: OrmEntity): String = (e.primaryKeyField :: e.dataFields).map(f => e.alias + "." + f.name).mkString(", ")
}
object FastOrmSql {
  implicit object DefaultFastOrmSql extends FastOrmSql
}