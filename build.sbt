scalaVersion := "2.11.8"

bintrayOrganization := Some("ovotech")
organization := "com.ovoenergy"
licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

scalaVersion := "2.11.11"
crossScalaVersions += "2.12.2"
releaseCrossBuild := true

resolvers += Resolver.bintrayRepo("ovotech", "maven")
resolvers += Resolver.sonatypeRepo("snapshots") // for scalacheck-shapeless
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.0.0",
  "com.chuusai" %% "shapeless" % "2.3.2",
    "com.ovoenergy" %% "comms-kafka-messages" % "1.66",
  "org.parboiled" %% "parboiled" % "2.1.3",
  "com.github.jknack" % "handlebars" % "4.0.6",
  "com.amazonaws" % "aws-java-sdk-s3" % "1.11.57",
  "org.slf4j" % "slf4j-api" % "1.7.5",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "com.github.ben-manes.caffeine" % "caffeine" % "2.4.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % Test,
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.8" % Test,
  "com.ironcorelabs" %% "cats-scalatest" % "2.2.0" % Test
)
testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck,
  "-minSize", "1",
  "-maxSize", "3",
  "-minSuccessfulTests", "100",
  "-maxDiscardRatio", "10",
  "-workers", "1"
)

// Make ScalaTest write test reports that CirceCI understands
val testReportsDir = sys.env.getOrElse("CI_REPORTS", "target/reports")
testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-o", "-u", testReportsDir)

tutSettings
tutTargetDirectory := baseDirectory.value

// scalafmt
val scalafmtAll = taskKey[Unit]("Run scalafmt in non-interactive mode with no arguments")
scalafmtAll := {
  import org.scalafmt.bootstrap.ScalafmtBootstrap
  streams.value.log.info("Running scalafmt ...")
  ScalafmtBootstrap.main(Seq("--non-interactive"))
  streams.value.log.info("Done")
}
(compile in Compile) := (compile in Compile).dependsOn(scalafmtAll).value

