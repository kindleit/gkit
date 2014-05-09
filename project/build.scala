import sbt._
import Keys._

object build extends Build {

  lazy val defaultSettings = Defaults.defaultSettings ++ Seq(
    version            := "0.1.2-SNAPSHOT",
    organization       := "com.kindleit",
    scalaVersion       := "2.10.4",
    scalacOptions     ++= Seq("-feature", "-language:implicitConversions", "-language:reflectiveCalls", "-language:higherKinds"),
    resolvers         ++= Seq(
      "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
      "Sonatype Snapshots"  at "http://oss.sonatype.org/content/repositories/snapshots/",
      "kindleit"            at "http://mvn.kitsd.com/repo/"),
    publishMavenStyle := true,
    credentials       += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    publishTo         := {
      val kitsd = "http://mvn.kitsd.com/"
      if (version.value.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at kitsd + "snapshots")
      else
        Some("releases"  at kitsd + "releases")
    })

  val jodaTime      = "joda-time"         %  "joda-time"        % "2.3"
  val jodaConvert   = "org.joda"          %  "joda-convert"     % "1.6"
  val specs2        = "org.specs2"        %% "specs2"           % "2.3.10-scalaz-7.1.0-M6" % "test"
  val scalaz        = "org.scalaz"        %% "scalaz-core"      % "7.1.0-M6"
  val scalazEffect  = "org.scalaz"        %% "scalaz-effect"    % "7.1.0-M6"
  val shapeless     = "com.chuusai"       %  "shapeless_2.10.4" % "2.0.0"
  val reactivemongo = "org.reactivemongo" %% "reactivemongo"    % "0.10.0"
  val play          = "com.typesafe.play" %% "play"             % "2.2.2"  % "provided"
  val playJSON      = "com.typesafe.play" %% "play-json"        % "2.2.2"
  val scalaReflect  = "org.scala-lang"    %  "scala-reflect"    % "2.10.3"

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
      libraryDependencies ++= Seq(scalaReflect, scalaz, shapeless)))

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
