lazy val scalaVersions = Seq("3.1.0", "2.13.7", "2.12.15")

ThisBuild / scalaVersion := scalaVersions.head
ThisBuild / versionScheme := Some("early-semver")

lazy val commonSettings: SettingsDefinition = Def.settings(
  organization := "de.lolhens",
  version := {
    val Tag = "refs/tags/(.*)".r
    sys.env.get("CI_VERSION").collect { case Tag(tag) => tag }
      .getOrElse("0.0.1-SNAPSHOT")
  },

  licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0")),

  homepage := Some(url("https://github.com/LolHens/http4s-errors")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/LolHens/http4s-errors"),
      "scm:git@github.com:LolHens/http4s-errors.git"
    )
  ),
  developers := List(
    Developer(id = "LolHens", name = "Pierre Kisters", email = "pierrekisters@gmail.com", url = url("https://github.com/LolHens/"))
  ),

  libraryDependencies ++= Seq(
    "ch.qos.logback" % "logback-classic" % "1.2.10" % Test,
    "de.lolhens" %%% "munit-tagless-final" % "0.2.0" % Test,
    "org.scalameta" %%% "munit" % "0.7.29" % Test,
  ),

  testFrameworks += new TestFramework("munit.Framework"),

  libraryDependencies ++= virtualAxes.?.value.getOrElse(Seq.empty).collectFirst {
    case VirtualAxis.ScalaVersionAxis(version, _) if version.startsWith("2.") =>
      compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
  },

  Compile / doc / sources := Seq.empty,

  publishMavenStyle := true,

  publishTo := sonatypePublishToBundle.value,

  credentials ++= (for {
    username <- sys.env.get("SONATYPE_USERNAME")
    password <- sys.env.get("SONATYPE_PASSWORD")
  } yield Credentials(
    "Sonatype Nexus Repository Manager",
    "oss.sonatype.org",
    username,
    password
  )).toList
)

name := (core.projectRefs.head / name).value

val V = new {
  val http4s = "0.23.7"
}

lazy val root: Project =
  project
    .in(file("."))
    .settings(commonSettings)
    .settings(
      publishArtifact := false,
      publish / skip := true
    )
    .aggregate(core.projectRefs: _*)

lazy val core = projectMatrix.in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "http4s-errors",

    libraryDependencies ++= Seq(
      "org.slf4j" % "slf4j-api" % "1.7.32",
      "org.typelevel" %%% "cats-effect" % "3.3.3",
      "org.http4s" %%% "http4s-core" % V.http4s,
      "org.http4s" %%% "http4s-dsl" % V.http4s % Test,
    ),
  )
  .jvmPlatform(scalaVersions)
  .jsPlatform(scalaVersions)
