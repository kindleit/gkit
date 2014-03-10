import sbt._
import Keys._

object build extends Build {

  lazy val defaultSettings = Defaults.defaultSettings ++ Seq(
    version            := "0.1.0-SNAPSHOT",
    organization       := "com.kindleit",
    scalaVersion       := "2.10.3",
    scalacOptions     ++= Seq("-feature", "-language:reflectiveCalls", "-language:higherKinds"),
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

  val jodaTime      = "joda-time"         %  "joda-time"     % "2.3"
  val jodaConvert   = "org.joda"          %  "joda-convert"  % "1.6"
  val scalaz        = "org.scalaz"        %% "scalaz-core"   % "7.1.0-SNAPSHOT"
  val shapeless     = "com.chuusai"       %  "shapeless"     % "2.0.0-SNAPSHOT" cross CrossVersion.full changing()
  val reactivemongo = "org.reactivemongo" %% "reactivemongo" % "0.10.0"
  val play          = "com.typesafe.play" %% "play"          % "2.2.2"     % "provided"
  val playJSON      = "com.typesafe.play" %% "play-json"     % "2.2.2"

  lazy val gkit = Project(
    id        = "gkit",
    base      = file("."),
    settings  = defaultSettings,
    aggregate = Seq(gPickler, gMongo, playGJSON, playGResource))

  lazy val gPickler = Project(
    id       = "gpickler",
    base     = file("gpickler"),
    settings = defaultSettings ++ Seq(
      name                := "gpickler",
      libraryDependencies ++= Seq(scalaz, shapeless)))

  lazy val gMongo = Project(
    id       = "gmongo",
    base     = file("gmongo"),
    settings = defaultSettings ++ Seq(
      name                := "gmongo",
      libraryDependencies ++= Seq(jodaTime, jodaConvert, scalaz, shapeless, reactivemongo)),
    dependencies = Seq(gPickler))

  lazy val playGJSON = Project(
    id       = "play-gjson",
    base     = file("play-gjson"),
    settings = defaultSettings ++ Seq(
      name                := "play-gjson",
      libraryDependencies ++= Seq(jodaTime, jodaConvert, scalaz, shapeless, playJSON)),
    dependencies = Seq(gPickler))

  lazy val playGResource = Project(
    id       = "play-gresource",
    base     = file("play-gresource"),
    settings = defaultSettings ++ Seq(
      name                := "play-gresource",
      libraryDependencies ++= Seq(jodaTime, jodaConvert, scalaz, play)),
    dependencies = Seq(gMongo, playGJSON))
}
