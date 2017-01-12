scalaVersion := "2.11.8"

resolvers += Resolver.bintrayRepo("ovotech", "maven")
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "0.8.1",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "com.ovoenergy" %% "comms-kafka-messages" % "0.0.29",
  "org.parboiled" %% "parboiled" % "2.1.3",
  "org.scalatest" %% "scalatest" % "3.0.1"
)
