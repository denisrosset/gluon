lazy val scalaVersions: Map[String, String] = Map("2.10" -> "2.10.6", "2.11" -> "2.11.8", "2.12" -> "2.12.1")

lazy val scalaTestVersion = "3.0.1"

name := "gluon"

organization := "com.faacets"

scalaVersion := scalaVersions("2.12")

scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation", "-language:implicitConversions")

licenses := Seq("MIT" -> url("http://opensource.org/licenses/mit-license.php"))

homepage := Some(url("https://github.com/denisrosset/gluon"))

bintrayRepository := "maven"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % scalaVersions("2.12"),
  "org.scala-lang" % "scala-reflect" % scalaVersions("2.12"),
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

packAutoSettings

packMakeTemplate := "Makefile.mustache"
