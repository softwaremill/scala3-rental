scalaVersion := "3.1.0"

val tapirVersion = "0.19.0-M12"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-generic" % "0.14.1",
  "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % tapirVersion,
  "com.softwaremill.sttp.tapir" %% "tapir-openapi-docs" % tapirVersion,
  "com.softwaremill.sttp.tapir" %% "tapir-openapi-circe-yaml" % tapirVersion,
  "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui" % tapirVersion,
  "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % tapirVersion,
  "ch.qos.logback" % "logback-classic" % "1.2.6"
)
