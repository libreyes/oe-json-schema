organization in ThisBuild := "uk.org.openeyes"

version in ThisBuild := "0.0.1-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.4"

scalacOptions in ThisBuild ++= Seq("-feature")

libraryDependencies in ThisBuild ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.11.6" % Test,
  "org.scalatest" %% "scalatest" % "2.2.2" % Test
)

testOptions in Test in ThisBuild += Tests.Argument("-oW")

lazy val core = project.in(file("core")).
  settings(
    name := "oe-json-schema-core",
    libraryDependencies ++= Seq(
      "org.json4s" %% "json4s-core" % "3.2.11",
      "org.json4s" %% "json4s-native" % "3.2.11" % Test
    )
  )

lazy val doc = project.in(file("doc")).
  dependsOn("core").
  settings(
    name := "oe-json-schema-doc",
    libraryDependencies ++= Seq(
      "org.json4s" %% "json4s-native" % "3.2.11"
    )
  )
