//package one.xingyi.cddlegacy
//
//import java.sql.ResultSet
//
//import one.xingyi.cddutilities.{Batcher, CddSpec, Jdbc, ReadAndBatch}
//import org.apache.commons.dbcp2.BasicDataSource
//import org.scalatest.BeforeAndAfterAll
//
//class JdbcSpec extends CddSpec with BeforeAndAfterAll with Jdbc {
//
//  val ds = new BasicDataSource
//  ds.setDriverClassName("org.h2.Driver")
//  ds.setUrl("jdbc:h2:~/test")
//  ds.setUsername("sa")
//  ds.setPassword("")
//
//  override protected def afterAll(): Unit = {
//    ds.close()
//    super.afterAll()
//  }
//
//  behavior of "JDBC"
//
//  def toLegacyData(rs: ResultSet) = LegacyData(rs.getInt("id"), rs.getString("situation"), rs.getString("result"))
//  def toLegacyResult(rs: ResultSet) = LegacyResult(rs.getInt("id"), Option(rs.getString("result")))
//  def setup(i: Int) = {
//    executeSql("drop table testsource if exists")(ds)
//    executeSql("drop table testresult if exists")(ds)
//    executeSql("create table testsource (id INT, situation VARCHAR(255), result VARCHAR(255));")(ds)
//    executeSql("create table testresult (id INT, result VARCHAR(255));")(ds)
//    1 to i foreach { i => executeSql(s"""insert into  testsource (id , situation , result ) values ($i, 'sit$i', 'result$i');""")(ds) }
//  }
//  it should "drop create  tables" in {
//    setup(1)
//    val x = getValue("select * from testsource;")(toLegacyData)(ds)
//    x shouldBe LegacyData(1, "sit1", "result1")
//  }
//
//  it should "batch things" in {
//    setup(6)
//    getValue("select count(*) from testsource")(rs => rs.getInt(1))(ds) shouldBe 6
//    val reader = new ReadAndBatch[LegacyData[String, String], LegacyResult]("select * from testsource", toLegacyData, lr => LegacyResult(lr.id, if (lr.id % 2 == 0) Some(s"Result: ${lr.id}") else None),
//      s"insert into testresult values (?,?)", lr => List(lr.id.toString, lr.failure.orNull), 5)
//    reader.doIt(ds)
//    getValue("select count(*) from testresult")(rs => rs.getInt(1))(ds) shouldBe 6
//    val x = getList("select * from testresult")(toLegacyResult)(ds)
//    x shouldBe List(LegacyResult(1, None), LegacyResult(2, Some("Result: 2")), LegacyResult(3, None), LegacyResult(4, Some("Result: 4")), LegacyResult(5, None), LegacyResult(6, Some("Result: 6")))
//
//  }
//
//}
