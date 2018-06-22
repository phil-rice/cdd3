package one.xingyi.cddlegacy

import java.sql.ResultSet

import javax.sql.DataSource
import one.xingyi.cddorm._
import one.xingyi.cddutilities.{CddSpec, ClosableM, Jdbc, SimpleClosable}
import org.apache.commons.dbcp2.BasicDataSource
import org.scalatest.BeforeAndAfterAll
import one.xingyi.cddutilities.ClosableLanguage._

import scala.collection.immutable
import scala.language.higherKinds

class AbstractJdbcSpec[M[_] : ClosableM] extends CddSpec with BeforeAndAfterAll with Jdbc {

  val ds = new BasicDataSource
  ds.setDriverClassName("org.h2.Driver")
  ds.setUrl("jdbc:h2:~/test")
  ds.setUsername("sa")
  ds.setPassword("")

  override protected def afterAll(): Unit = {
    ds.close()
    super.afterAll()
  }

  behavior of "JDBC"

  def toLegacyData(rs: ResultSet) = LegacyData(rs.getInt("id"), rs.getString("situation"), rs.getString("result"))
  def toLegacyResult(rs: ResultSet) = LegacyResult(rs.getInt("id"), Option(rs.getString("result")))
  def setup(i: Int) = {
    executeSql("drop table testsource if exists") apply ds
    executeSql("drop table testresult if exists") apply ds
    executeSql("create table testsource (id INT, situation VARCHAR(255), result VARCHAR(255));") apply ds
    executeSql("create table testresult (id INT, result VARCHAR(255));") apply ds
    1 to i foreach { i => executeSql(s"""insert into  testsource (id , situation , result ) values ($i, 'sit$i', 'result$i');""") apply ds }
  }


  it should
  "drop create  tables" in {
    setup(1)
    val x = getValue("select * from testsource;")(toLegacyData) apply ds
    x shouldBe LegacyData(1, "sit1", "result1")
  }

  it should "batch things" in {
    setup(7)
    getValue("select count(*) from testsource")(rs => rs.getInt(1)) apply ds shouldBe 7
    def fn(legacyData: LegacyData[String, String]) = LegacyResult(legacyData.id, if (legacyData.id < 3) None else Some(s"id: ${legacyData.id}"))
    val x = process[M, LegacyData[String, String], LegacyResult](5)("select * from testsource", toLegacyData)(s"insert into testresult values (?,?)", lr => List(lr.id.toString, lr.failure.orNull)) _
    x(fn _)(ds).close()
    getValue("select count(*) from testresult")(rs => rs.getInt(1)) apply (ds) shouldBe 7
    val list = getList("select * from testresult")(toLegacyResult) apply (ds)
    list shouldBe List(LegacyResult(1, None), LegacyResult(2, None), LegacyResult(3, Some("id: 3")), LegacyResult(4, Some("id: 4")), LegacyResult(5, Some("id: 5")), LegacyResult(6, Some("id: 6")), LegacyResult(7, Some("id: 7")))
  }

  it should "create temporary tables" in {
    setup(7)
    executeSql("create temporary table testtemp as select * from testsource") apply ds
    val x = getList("select * from testsource;")(toLegacyData) apply ds
    x.foreach(println)

  }

  behavior of "FastOrm"

  val address = OneToManyEntity("Address", "A", IntField("aid"), IntField("personid"), List(StringField("add")), List())
  val phone = OneToManyEntity("Phone", "Ph", IntField("aid"), IntField("personid"), List(StringField("phoneNo")), List())
  val main = MainEntity("Person", "P", IntField("pid"), List(StringField("name")), List(address, phone))

  case class Address(add: String)
  case class Phone(phoneNo: String)
  case class Person(name: String, address: List[Address], phones: List[Phone])

  implicit class MapOfListsPimper[K, V](map: Map[K, List[V]]) {
    def addToList(kv: (K, V)): Map[K, List[V]] = kv match {case (k, v) => map.get(k).fold(map + (k -> List[V](v)))(list => map + (k -> (list :+ v)))}
  }

  def toMap[X](list: List[List[AnyRef]])(fn: List[AnyRef] => X): Map[Any, List[X]] =
    list.foldLeft[Map[Any, List[X]]](Map()) { case (acc, key :: _ :: values) => acc addToList key -> fn(values) }

  implicit val maker: OrmMaker[Person] = { map =>
    val aList = toMap(map(address))(list => Address(list(0).toString))
    val phoneList = toMap(map(phone))(list => Phone(list(0).toString))
    val result: List[Person] = map(main).map { case id :: name :: _ => Person(name.toString, aList.getOrElse(id, Nil), phoneList.getOrElse(id, Nil)) }
    result
  }


  it should "do stuff" in {
    def printIt = { s: Any => println(s) }
    OrmStrategies.dropTables.map(printIt).walk(main)
    OrmStrategies.createTables.map(printIt).walk(main)
    OrmStrategies.dropTempTables.map(printIt).walk(main)
    OrmStrategies.createTempTables(BatchDetails(1000, 3)).map(printIt).walk(main) //
    OrmStrategies.drainTempTables.map(printIt) walk (main)

    def execute = { s: String => executeSql(s) apply ds }
    def query = { s: String => getList(s) { rs: ResultSet => (1 to rs.getMetaData.getColumnCount).toList.map(rs.getObject) } apply ds }

    OrmStrategies.dropTables.map(execute).walk(main)
    OrmStrategies.createTables.map(execute).walk(main)
    executeSql(s"""insert into  Person (pid, name ) values (1, 'Phil');""") apply ds
    executeSql(s"""insert into  Address (aid, personid, add ) values (1, 1, 'Phils first address');""") apply ds
    executeSql(s"""insert into  Address (aid, personid, add ) values (2, 1, 'Phils second address');""") apply ds
    executeSql(s"""insert into  Person (pid, name ) values (2, 'Bob');""") apply ds
    executeSql(s"""insert into  Person (pid, name ) values (3, 'Jill');""") apply ds
    executeSql(s"""insert into  Address (aid, personid, add ) values (3, 3, 'Jills first address');""") apply ds
    OrmStrategies.dropTempTables.map(execute).walk(main)
    OrmStrategies.createTempTables(BatchDetails(2, 0)).map(execute).walk(main)
    OrmStrategies.drainTempTables.map(query).map(printIt).walk(main)
    val x: Map[OrmEntity, List[List[AnyRef]]] = OrmStrategies.drainTempTables.map(query).walk(main).toMap
    val y: Seq[Any] = maker(x)
    y.foreach(println)


    val reader = new FastReaderImpl[M, Person](ds)
    reader(main).foreach(println)

  }

}

class JdbcSpec extends AbstractJdbcSpec[SimpleClosable]
