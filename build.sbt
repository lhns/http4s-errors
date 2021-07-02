organization := "de.lolhens"
name := "http4s-errors"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.13.6"
crossScalaVersions := Seq("2.12.14", scalaVersion.value)

ThisBuild / versionScheme := Some("early-semver")

licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0"))

homepage := Some(url("https://github.com/LolHens/http4s-errors"))
scmInfo := Some(
  ScmInfo(
    url("https://github.com/LolHens/http4s-errors"),
    "scm:git@github.com:LolHens/http4s-errors.git"
  )
)
developers := List(
  Developer(id = "LolHens", name = "Pierre Kisters", email = "pierrekisters@gmail.com", url = url("https://github.com/LolHens/"))
)

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "1.7.31",
  "org.typelevel" %% "cats-effect" % "2.5.1",
  "org.http4s" %% "http4s-core" % "0.21.24",
  "org.scalameta" %% "munit" % "0.7.27" % Test,
  "de.lolhens" %% "munit-tagless-final" % "0.1.3" % Test,
  "org.http4s" %% "http4s-dsl" % "0.21.24" % Test,
)

testFrameworks += new TestFramework("munit.Framework")

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

Compile / doc / sources := Seq.empty

version := {
  val tagPrefix = "refs/tags/"
  sys.env.get("CI_VERSION").filter(_.startsWith(tagPrefix)).map(_.drop(tagPrefix.length)).getOrElse(version.value)
}

publishMavenStyle := true

publishTo := sonatypePublishToBundle.value

credentials ++= (for {
  username <- sys.env.get("SONATYPE_USERNAME")
  password <- sys.env.get("SONATYPE_PASSWORD")
} yield Credentials(
  "Sonatype Nexus Repository Manager",
  "oss.sonatype.org",
  username,
  password
)).toList
