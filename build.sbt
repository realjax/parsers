name := """parsercombinators"""

version := "1.0"

scalaVersion := "2.11.1"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.4.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.6" % "test"
)

