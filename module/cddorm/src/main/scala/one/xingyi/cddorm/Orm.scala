package one.xingyi.cddorm
import java.sql.ResultSet

import javax.sql.DataSource
import one.xingyi.cddutilities.ClosableM

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

object Streams extends Streams
trait Streams {
  def unfold[S, A](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => a #:: unfold(s)(f)
    case None => Stream.empty[A]
  }
  def unfoldList[S, A](z: S)(f: S => Option[(List[A], S)]): Stream[A] = f(z) match {
    case Some((a, s)) => a.toStream #::: unfoldList(s)(f)
    case None => Stream.empty[A]
  }
  def unfoldIndexedList[S, A](n: Int, z: S)(f: (Int, S) => Option[(List[A], S)]): Stream[A] = {f(n, z) match {
    case Some((a, s)) => a.toStream #::: unfoldIndexedList[S, A](n + 1, s)(f)
    case None => Stream.empty[A]
  }}

}
trait FastReader[T] extends (MainEntity => Stream[T])
trait OrmMaker[T] extends (Map[OrmEntity, List[List[AnyRef]]] => List[T])

//TODO Clean this. Remove M and sqlops and just pass in some functions that do it...
class FastReaderImpl[M[_] : ClosableM, T](ds: DataSource)(implicit ormMaker: OrmMaker[T], sqlOps: SqlOps) extends FastReader[T] {
  import one.xingyi.cddutilities.Jdbc._

  def execute = { s: String => executeSql(s) apply ds }
  def query = { s: String => getList(s) { rs: ResultSet => (1 to rs.getMetaData.getColumnCount).toList.map(rs.getObject) } apply ds }


  def makeBatch(n: Int, main: MainEntity): Option[( List[T],MainEntity)] = {
    OrmStrategies.dropTempTables.map(execute).walk(main)
    OrmStrategies.createTempTables(BatchDetails(2, n)).map(execute).walk(main)
    val data: Map[OrmEntity, List[List[AnyRef]]] = OrmStrategies.drainTempTables.map(query).walk(main).toMap
    val list = ormMaker(data)
    if (list.size == 0) None else Some((list, main))
  }
  override def apply(mainEntity: MainEntity): Stream[T] = Streams.unfoldIndexedList(0, mainEntity)(makeBatch)
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


