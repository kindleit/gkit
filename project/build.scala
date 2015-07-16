import sbt._
import Keys._
import sbtunidoc.Plugin.unidocSettings
import bintray.Plugin.bintraySettings
import bintray.Keys._

object build extends Build {

  val scalaV = "2.11.7"

  lazy val defaultSettings = Defaults.defaultSettings ++ unidocSettings ++ bintraySettings ++ Seq(
    version            := "0.4.0",
    organization       := "com.kindleit",
    scalaVersion       := scalaV,
    scalacOptions     ++= Seq(
      "-feature",
      "-optimize",
      "-language:implicitConversions",
      "-language:reflectiveCalls",
      "-language:higherKinds",
      "-target:jvm-1.8",
      "-Ybackend:GenBCode",
      "-Ydelambdafy:method",
      "-Yopt:l:classpath"
    ),
    resolvers         ++= Seq(
      "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
      "Sonatype Snapshots"  at "https://oss.sonatype.org/content/repositories/snapshots/"),
    publishMavenStyle := true,
    bintrayOrganization in bintray := Some("kitsd"),
    repository in bintray := "releases",
    licenses          += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html")))

  val scalaReflect  = "org.scala-lang"    %  "scala-reflect"    % scalaV
  val java8Compat   = "org.scala-lang.modules" %% "scala-java8-compat" % "0.5.0"
  val jodaTime      = "joda-time"         %  "joda-time"        % "2.5"
  val jodaConvert   = "org.joda"          %  "joda-convert"     % "1.7"
  val scalaz        = "org.scalaz"        %% "scalaz-core"      % "7.1.0"
  val scalazEffect  = "org.scalaz"        %% "scalaz-effect"    % "7.1.0"
  val shapeless     = "com.chuusai"       %%  "shapeless"       % "2.0.0"
  val reactivemongo = "org.reactivemongo" %% "reactivemongo"    % "0.10.5.0.akka23"
  val specs2        = "org.specs2"        %% "specs2-core"      % "2.4.6" % "test"
  val play          = "com.typesafe.play" %% "play"             % "2.3.0" % "provided"
  val playJSON      = "com.typesafe.play" %% "play-json"        % "2.3.0" % "provided"

  lazy val gkit = Project(
    id        = "gkit",
    base      = file("."),
    settings  = defaultSettings,
    aggregate = Seq(core, mongo, sql, playGJSON, playGResource, playGResourceMongo, playGResourceSQL))

  lazy val core = Project(
    id       = "core",
    base     = file("core"),
    settings = defaultSettings ++ Seq(
      name                := "gkit-core",
      libraryDependencies ++= Seq(java8Compat, scalaReflect, scalaz, shapeless)))

  lazy val mongo = Project(
    id       = "mongo",
    base     = file("mongo"),
    settings = defaultSettings ++ Seq(
      name                := "gkit-mongo",
      libraryDependencies ++= Seq(scalaReflect, jodaTime, jodaConvert, specs2, scalaz, shapeless, reactivemongo)),
    dependencies = Seq(core))

  lazy val sql = Project(
    id       = "sql",
    base     = file("sql"),
    settings = defaultSettings ++ Seq(
      name                := "gkit-sql",
      libraryDependencies ++= Seq(scalaReflect, jodaTime, jodaConvert, scalaz, scalazEffect, shapeless)),
    dependencies = Seq(core))

  lazy val playGJSON = Project(
    id       = "play-gjson",
    base     = file("play-gjson"),
    settings = defaultSettings ++ Seq(
      name                := "play-gjson",
      libraryDependencies ++= Seq(jodaTime, jodaConvert, specs2, scalaz, shapeless, playJSON)),
    dependencies = Seq(core))

  lazy val playGResource = Project(
    id       = "play-gresource",
    base     = file("play-gresource"),
    settings = defaultSettings ++ Seq(
      name                := "play-gresource",
      libraryDependencies ++= Seq(jodaTime, jodaConvert, scalaz, shapeless, play)),
    dependencies = Seq(core, playGJSON))

  lazy val playGResourceMongo = Project(
    id       = "play-gresource-mongo",
    base     = file("play-gresource-mongo"),
    settings = defaultSettings ++ Seq(
      name                := "play-gresource-mongo",
      libraryDependencies ++= Seq(jodaTime, jodaConvert, scalaz, play)),
    dependencies = Seq(mongo, playGJSON, playGResource))

  lazy val playGResourceSQL = Project(
    id       = "play-gresource-sql",
    base     = file("play-gresource-sql"),
    settings = defaultSettings ++ Seq(
      name                := "play-gresource-sql",
      libraryDependencies ++= Seq(jodaTime, jodaConvert, scalaz, play)),
    dependencies = Seq(sql, playGJSON, playGResource))
}
