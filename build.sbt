

val versions = new {
  val cdd = "0.1"
  val scala = "2.12.2"
  //  val scala = "2.12.1"
  val finatra = "18.2.0"
  val scalatest = "3.0.5"
  val mockito = "1.10.19"
  val guice = "4.0"
  val play = "2.5.12"
  val scalapact = "2.1.3"
  val junit = "4.12"
  val json4s = "3.5.3"
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

val cddutilities = (project in file("module/cddutilities")).
  settings(utilitiesSettings)

val cddscenario = (project in file("module/cddscenario")).
  dependsOn(cddutilities % "test->test;compile->compile").
  settings(publishSettings)

val cddengine = (project in file("module/cddengine")).
  dependsOn(cddutilities % "test->test;compile->compile").
  dependsOn(cddscenario % "test->test;compile->compile").
  settings(publishSettings)

val cddexamples = (project in file("module/cddexamples")).
  dependsOn(cddutilities % "test->test;compile->compile").
  dependsOn(cddengine % "test->test;compile->compile").
  settings(publishSettings)


val cddtest = (project in file("module/cddtest")).
  dependsOn(cddutilities % "test->test;compile->compile").
  dependsOn(cddengine % "test->test;compile->compile").
  dependsOn(cddscenario % "test->test;compile->compile").
  settings(publishSettings)

val cdd3 = (project in file(".")).
  settings(publishSettings).
  settings(publishArtifact := false).
  aggregate(cddengine, cddutilities, cddscenario, cddtest)
