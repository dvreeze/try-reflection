
val scalaVer = "3.3.0"
val crossScalaVer = Seq(scalaVer)

ThisBuild / description  := "Trying out Java reflection"
ThisBuild / organization := "eu.cdevreeze.tryreflection"
ThisBuild / version      := "0.1.0-SNAPSHOT"

ThisBuild / versionScheme := Some("strict")

ThisBuild / scalaVersion       := scalaVer
ThisBuild / crossScalaVersions := crossScalaVer

ThisBuild / semanticdbEnabled := false // do not enable SemanticDB

ThisBuild / scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

ThisBuild / publishMavenStyle := true

ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) {
    Some("snapshots" at nexus + "content/repositories/snapshots")
  } else {
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }
}

ThisBuild / pomExtra := pomData
ThisBuild / pomIncludeRepository := { _ => false }

val circeVersion = "0.14.1"

ThisBuild / libraryDependencies += "io.circe" %% "circe-core" % circeVersion
ThisBuild / libraryDependencies += "io.circe" %% "circe-generic" % circeVersion
ThisBuild / libraryDependencies += "io.circe" %% "circe-parser" % circeVersion

ThisBuild / libraryDependencies += "org.burningwave" % "core" % "12.64.2"

ThisBuild / Test / fork := true

lazy val root = project.in(file("."))
  .settings(
    name                 := "tryreflection",
    publish              := {},
    publishLocal         := {},
    publishArtifact      := false,
    Keys.`package`       := file(""))

lazy val pomData =
  <url>https://github.com/dvreeze/try-reflection</url>
  <licenses>
    <license>
      <name>Apache License, Version 2.0</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
      <distribution>repo</distribution>
      <comments>Try-zio is licensed under Apache License, Version 2.0</comments>
    </license>
  </licenses>
  <scm>
    <connection>scm:git:git@github.com:dvreeze/try-reflection.git</connection>
    <url>https://github.com/dvreeze/try-reflection.git</url>
    <developerConnection>scm:git:git@github.com:dvreeze/try-reflection.git</developerConnection>
  </scm>
  <developers>
    <developer>
      <id>dvreeze</id>
      <name>Chris de Vreeze</name>
      <email>chris.de.vreeze@caiway.net</email>
    </developer>
  </developers>
