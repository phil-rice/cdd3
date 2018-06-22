

val versions = new {
  val cdd = "0.1"
  val scala = "2.12.2"
  val scalatest = "3.0.5"
  val mockito = "1.10.19"
  val junit = "4.12"
  val json4s = "3.5.3"
  val mustache = "0.9.5"
}

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

lazy val commonSettings = Seq(
  version := versions.cdd,
  scalaVersion := versions.scala,
  organization := "one.xingyi",
  publishMavenStyle := true,
  scalaVersion := versions.scala,
  scalacOptions ++= Seq("-feature"),
  libraryDependencies += "org.mockito" % "mockito-all" % versions.mockito % "test",
  libraryDependencies += "org.scalatest" %% "scalatest" % versions.scalatest % "test"
)

lazy val publishSettings = commonSettings ++ Seq(
  pomIncludeRepository := { _ => false },
  publishMavenStyle := true,
  publishArtifact in Test := false,
  licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php")),
  homepage := Some(url("http://example.com")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/phil-rice/pact-stubber"),
      "scm:git@github.com/phil-rice/pact-stubber.git"
    )
  ),
  developers := List(
    Developer(
      id = "phil",
      name = "Phil Rice",
      email = "phil.rice@iee.org",
      url = url("https://www.linkedin.com/in/phil-rice-53959460")
    )
  ),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  })


lazy val utilitiesSettings = publishSettings ++ Seq(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % versions.scala
)

lazy val json4sSettings = publishSettings ++ Seq(
  libraryDependencies += "org.json4s" %% "json4s-native" % versions.json4s
)
lazy val mustacheSettings = publishSettings ++ Seq(
  libraryDependencies += "com.github.spullara.mustache.java" % "scala-extensions-2.11" % versions.mustache
)
lazy val junitSettings = publishSettings ++ Seq(
  libraryDependencies += "junit" % "junit" % versions.junit
)
lazy val apacheDbcp2Settings = publishSettings ++ Seq(
  libraryDependencies += "org.apache.commons" % "commons-dbcp2" % "2.3.0",
  libraryDependencies += "com.h2database" % "h2" % "1.4.197"
)

lazy val scalatestSeetings = publishSettings ++ Seq(
  libraryDependencies += "org.scalatest" %% "scalatest" % versions.scalatest
)

val cddutilities = (project in file("module/cddutilities")).
  settings(utilitiesSettings)

val cddscripts = (project in file("module/cddscripts")).
  settings(utilitiesSettings)

val cddmustache = (project in file("module/cddmustache")).
  dependsOn(cddutilities % "test->test;compile->compile").
  aggregate(cddutilities).
  settings(mustacheSettings)

val cddjson4s = (project in file("module/cddjson4s")).
  dependsOn(cddutilities % "test->test;compile->compile").
  settings(json4sSettings)

val cddscenario = (project in file("module/cddscenario")).
  dependsOn(cddutilities % "test->test;compile->compile").
  settings(publishSettings)

val cddengine = (project in file("module/cddengine")).
  dependsOn(cddutilities % "test->test;compile->compile").
  dependsOn(cddscenario % "test->test;compile->compile").
  settings(publishSettings)

val cddscalatest = (project in file("module/cddscalatest")).
  dependsOn(cddutilities % "test->test;compile->compile").
  dependsOn(cddengine % "test->test;compile->compile").
  settings(scalatestSeetings)

val cddlegacy = (project in file("module/cddlegacy")).
  dependsOn(cddutilities % "test->test;compile->compile").
  dependsOn(cddengine % "test->test;compile->compile").
  settings(publishSettings)

val cddapachejdbc = (project in file("module/cddapachejdbc")).
  dependsOn(cddutilities % "test->test;compile->compile").
  settings(apacheDbcp2Settings)

val cddexamples = (project in file("module/cddexamples")).
  dependsOn(cddutilities % "test->test;compile->compile").
  dependsOn(cddengine % "test->test;compile->compile").
  dependsOn(cddjson4s % "test->test;compile->compile").
  dependsOn(cddscalatest % "test->test;compile->compile").
  dependsOn(cddmustache % "test->test;compile->compile").
  dependsOn(cddlegacy % "test->test;compile->compile").
  dependsOn(cddapachejdbc % "test->test;compile->compile").
  settings(publishSettings)

val cddtest = (project in file("module/cddtest")).
  dependsOn(cddutilities % "test->test;compile->compile").
  dependsOn(cddengine % "test->test;compile->compile").
  dependsOn(cddscenario % "test->test;compile->compile").
  dependsOn(cddjson4s % "test->test;compile->compile").
  dependsOn(cddlegacy % "test->test;compile->compile").
  dependsOn(cddapachejdbc % "test->test;compile->compile").
  settings(publishSettings)


val cdd3 = (project in file(".")).
  settings(publishSettings).
  settings(publishArtifact := false).
  aggregate(cddengine, cddutilities, cddscenario, cddtest, cddjson4s, cddmustache, cddscalatest, cddlegacy, cddapachejdbc, cddexamples)
