name := "slick-additions"

val settings = Seq(
  organization := "io.github.nafg",
  crossScalaVersions := Seq("2.11.8", "2.12.1"),
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq("-deprecation", "-unchecked")
)

lazy val `slick-additions-entity` =
  crossProject.crossType(CrossType.Pure)
    .settings(settings: _*)
lazy val `slick-additions-entity-jvm` = `slick-additions-entity`.jvm
lazy val `slick-additions-entity-js` = `slick-additions-entity`.js

lazy val `slick-additions` =
  (project in file("."))
    .dependsOn(`slick-additions-entity-jvm`)
    .aggregate(`slick-additions-entity-jvm`, `slick-additions-entity-js`)
    .settings(settings)
    .settings(
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
        "com.typesafe.slick" %% "slick" % "3.2.0",
        "org.scalatest" %% "scalatest" % "3.0.1" % "test",
        "com.h2database" % "h2" % "1.4.194" % "test",
        "ch.qos.logback" % "logback-classic" % "1.2.2" % "test"
      )
    )
