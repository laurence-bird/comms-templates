scalaVersion := "2.11.8"

resolvers += Resolver.bintrayRepo("ovotech", "maven")
resolvers += Resolver.sonatypeRepo("snapshots") // for scalacheck-shapeless
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "0.8.1",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "com.ovoenergy" %% "comms-kafka-messages" % "0.0.29",
  "org.parboiled" %% "parboiled" % "2.1.3",
  "com.github.jknack" % "handlebars" % "4.0.6",
  "com.amazonaws" % "aws-java-sdk-s3" % "1.11.57",
  "org.slf4j" % "slf4j-api" % "1.7.5",
  "org.scalatest" %% "scalatest" % "3.0.1" % Test,
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.5-SNAPSHOT" % Test,
  "com.ironcorelabs" %% "cats-scalatest" % "2.2.0" % Test
)
testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck,
  "-minSize", "1",
  "-maxSize", "3",
  "-minSuccessfulTests", "100",
  "-maxDiscardRatio", "10",
  "-workers", "1"
)
