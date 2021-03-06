name := """apiserver"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  "commons-io" % "commons-io" % "2.3",
  jdbc,
  anorm,
  cache,
  ws
)

javaOptions ++= Seq("-Xms128M", "-Xmx128",  "-XX:+CMSClassUnloadingEnabled")
