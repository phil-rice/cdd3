package one.xingyi.cddutilities.orm

import java.sql.PreparedStatement

import one.xingyi.cddutilities.jdbc.Jdbc._
import one.xingyi.cddutilities.functions.ClosableLanguage._
import one.xingyi.cddutilities.functions.ClosableM

import scala.language.higherKinds

trait ToWritableForm[T] extends (T => List[(OrmEntity, List[List[AnyRef]])])

trait Write[T] {
  //note we can use ps.getGeneratedKeys and add this result set to the closeables
  def insert(data: List[List[AnyRef]])(ps: PreparedStatement) = {data.foreach(addInsertItem(ps)); ps.executeBatch(); ps.clearBatch()}
  def addInsertItem(ps: PreparedStatement) = { l: List[AnyRef] => l.zipWithIndex.foreach { case (item, i) => ps.setObject(i + 1, item) } }

  def apply[M[_] : ClosableM](data: List[(String, List[List[AnyRef]])]) =
    data.foreach { case (sql, data) => prepare(sql) ==> insert(data) }

}
class WriteOrm {

}
