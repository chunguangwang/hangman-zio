val scalaVer = "2.13.4"

val Http4sVersion = "1.0.0-M21"
val http4sBlaze = "0.23.13"
val CirceVersion = "0.14.0-M5"
lazy val settings = Seq(
  name := "zio-hangman",
  version := "1.0.0",
  scalaVersion := scalaVer,
  libraryDependencies ++= Seq(
    // https://mvnrepository.com/artifact/org.postgresql/postgresql
    "org.postgresql" % "postgresql" % "42.2.5",
    "org.http4s" %% "http4s-blaze-client" % Http4sVersion,
    "org.http4s" %% "http4s-blaze-server" % Http4sVersion,
    "org.http4s" %% "http4s-circe" % Http4sVersion,
    "io.circe" %% "circe-generic" % CirceVersion,
    "org.http4s" %% "http4s-dsl" % Http4sVersion,
    "org.http4s" %% "http4s-ember-server" % Http4sVersion,
    "org.http4s" %% "http4s-ember-client" % Http4sVersion
  ),
  libraryDependencies ++= Seq(
    "org.apache.spark" %% "spark-core" % "3.2.2",
    "org.apache.spark" %% "spark-sql" % "3.2.2",
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.13.0", // Updated to a version compatible with Scala 2.13
    "com.databricks" % "databricks-jdbc" % "2.6.36",
    "io.delta" %% "delta-core" % "2.4.0",
    "org.apache.hadoop" % "hadoop-aws" % "3.2.0",
    "com.amazonaws" % "aws-java-sdk-bundle" % "1.11.903",
    "org.apache.hadoop" % "hadoop-common" % "3.2.0",
    "com.typesafe.play" %% "play-json" % "2.9.2",
    "com.opencsv" % "opencsv" % "5.5.2"
  ),
  libraryDependencies += "dev.zio" %% "zio" % "2.1.3",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "log4cats-core" % "2.1.1", // Only if you want to Support Any Backend
    "org.typelevel" %% "log4cats-slf4j" % "2.1.1",
    "org.slf4j" % "slf4j-simple" % "1.7.30", // https://mvnrepository.com/artifact/org.tpolecat/natchez-log
    "org.tpolecat" %% "natchez-log" % "0.1.5"
  ),
  libraryDependencies += "org.typelevel" %% "cats-core" % "2.2.0",
  libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.5",
  libraryDependencies += "org.tpolecat" %% "natchez-core" % "0.1.5"
)

lazy val root = (project in file("."))
  .settings(settings)
