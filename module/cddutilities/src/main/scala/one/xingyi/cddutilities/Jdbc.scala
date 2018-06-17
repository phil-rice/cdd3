package one.xingyi.cddutilities
import java.sql.{Connection, PreparedStatement, ResultSet, Statement}
import java.util.concurrent.atomic.AtomicInteger

import javax.sql.DataSource
import one.xingyi.cddutilities.Batcher.jdbcInsert
import one.xingyi.cddutilities.language.AnyLanguage._
import one.xingyi.cddutilities.language.Closer
import one.xingyi.cddutilities.language.FunctionLanguage._

import scala.language.postfixOps

object Jdbc extends Jdbc


trait Jdbc {
  implicit val datasourceCloser: Closer[DataSource] = { c => } //bit of an issue here. We don't actually want to close the first one...
  implicit val connectionCloser: Closer[Connection] = { c => c.close() }
  implicit val statementCloser: Closer[Statement] = { s => s.close() }
  implicit val preparsedStatementCloser: Closer[PreparedStatement] = { s => s.close() }
  implicit val resultSetCloser: Closer[ResultSet] = { rs => rs.close() }

  val connection = { d: DataSource => d.getConnection }
  val statement = { c: Connection => c.createStatement() }
  def prepare(sql: String) = { c: Connection => c.prepareStatement(sql) }
  def toResultSet(sql: String) = { s: Statement => s.executeQuery(sql) }
  def execute(sql: String) = { s: Statement => s.execute(sql) }

  def executeSql(sql: String) = from[DataSource] make connection and statement and execute(sql)
  def getValue[X](sql: String)(fn: ResultSet => X) = from[DataSource] make (_.getConnection) and (_.createStatement) and { statement =>
    val resultSet = statement.executeQuery(sql)
    if (!resultSet.next()) throw new RuntimeException(s"No values for $sql")
    val result = fn(resultSet)
    if (resultSet.next()) throw new RuntimeException(s"Too many values for $sql")
    result
  }
  def getList[X](sql: String)(fn: ResultSet => X) = from[DataSource] make connection and statement and toResultSet(sql) thenDo { resultSet =>
    var list = List[X]()
    while (resultSet.next()) {list = fn(resultSet) :: list}
    list.reverse
  }
}

case class BatchConfig[T](batchSize: Int, prepare: T => Unit, flush: () => Unit)
class Batcher[T](batchConfig: BatchConfig[T], count: AtomicInteger = new AtomicInteger(0)) extends (T => Unit) {
  import batchConfig._
  override def apply(t: T): Unit = prepare(t) sideeffectTry (_ => count.tick(batchSize)(flush())) get
  def close = count ifNotZero flush()
}

import one.xingyi.cddutilities.Jdbc._
case class ReadAndBatch[From, To](readSql: String, readFn: ResultSet => From, fn: From => To, writeSql: String, preparer: To => List[Object], batchSize: Int) {
  val doIt: DataSource => Unit = {
    from[DataSource] make connection makeBoth statement and prepare(writeSql) and { case (readStatement, writeStatement) =>
      from[Statement] make toResultSet(readSql) thenDo { resultSet =>
        val batchInserter = jdbcInsert(batchSize, preparer)(writeStatement)
        while (resultSet.next()) {
          val from = readFn(resultSet)
          val to = fn(from)
          batchInserter(to)
        }
        batchInserter.close
      } apply readStatement
    }
  }
}


trait JdbcInserter[T] extends (Int => PreparedStatement => (T => List[Object]) => Unit)
object Batcher {
  def jdbcInsert[T](batchSize: Int, preparer: T => List[Object])(statement: PreparedStatement): Batcher[T] =
    new Batcher(BatchConfig(batchSize,
      { t => preparer(t).zipWithIndex.foreach { case (o, i) => statement.setObject(i + 1, o) }; statement.addBatch() },
      { () => statement.executeBatch() }))

}
