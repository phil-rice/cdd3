package one.xingyi.cddutilities
import java.sql.{Connection, PreparedStatement, ResultSet, Statement}
import java.util.concurrent.atomic.AtomicInteger

import javax.sql.DataSource
import one.xingyi.cddutilities.Batcher.jdbcInsert
import one.xingyi.cddutilities.Jdbc.{foreachResultSet, withConnection, withPreparedStatement}
import one.xingyi.cddutilities.language.AnyLanguage._
import one.xingyi.cddutilities.language.FunctionLanguage._

import scala.language.postfixOps

object Jdbc extends Jdbc
trait Jdbc {

  def withConnection[X](dataSource: DataSource)(fn: Connection => X): X = from(dataSource) make (_.getConnection) thenDo fn andCloseWith (_.close)

  def withStatement[X](fn: Statement => X)(implicit connection: Connection): X = from(connection) make (_.createStatement) thenDo fn andCloseWith (_.close)

  def withResultSet[X](sql: String)(fn: ResultSet => X)(implicit connection: Connection): X =
    from(connection) make (_.createStatement) andMake (_.executeQuery(sql)) thenDo fn andCloseWith (_.close) and (_.close)

  def foreachResultSet[X](sql: String)(transform: ResultSet => X)(block: X => Unit)(implicit connection: Connection): Unit =
    from(connection) make (_.createStatement) andMake (_.executeQuery(sql)) thenDo (resultSet => while (resultSet.next()) block(transform(resultSet))) andCloseWith (_.close) and (_.close)

  def withPreparedStatement[X](sql: String)(fn: PreparedStatement => X)(implicit connection: Connection): X =
    from(connection) make (_.prepareStatement(sql)) thenDo fn andCloseWith (_.close)
}

case class BatchConfig[T](batchSize: Int, prepare: T => Unit, flush: () => Unit)
class Batcher[T](batchConfig: BatchConfig[T], count: AtomicInteger = new AtomicInteger(0)) extends (T => Unit) {
  import batchConfig._
  override def apply(t: T): Unit = prepare(t) sideeffectTry (_ => count.tick(batchSize)(flush())) get
  def close = count ifNotZero flush()
}

case class ReadAndBatch[From, To](dataSource: DataSource)(readSql: String, readFn: ResultSet => From, fn: From => To, writeSql: String, preparer: To => List[Object], batchSize: Int) {
  def doit = withConnection(dataSource) { implicit connection => withPreparedStatement(writeSql) { implicit writeStatement => jdbcInsert(batchSize, preparer) applyTo foreachResultSet[To](readSql)(readFn andThen fn) } }
}

trait JdbcInserter[T] extends (Int => PreparedStatement => (T => List[Object]) => Unit)
object Batcher {
  def jdbcInsert[T](batchSize: Int, preparer: T => List[Object])(implicit statement: PreparedStatement): Batcher[T] =
    new Batcher(BatchConfig(batchSize,
      { t => preparer(t).zipWithIndex.foreach { case (o, i) => statement.setObject(i + 1, o) }; statement.addBatch() },
      { () => statement.executeBatch() }))

}
