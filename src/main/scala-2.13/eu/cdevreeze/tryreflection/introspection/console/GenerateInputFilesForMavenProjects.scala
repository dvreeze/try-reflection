/*
 * Copyright 2024-2024 Chris de Vreeze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package eu.cdevreeze.tryreflection.introspection.console

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, Printer, parser}
import org.burningwave.core.ManagedLogger
import org.burningwave.core.assembler.{ComponentContainer, ComponentSupplier}
import org.burningwave.core.io.PathHelper

import java.io.{ByteArrayOutputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps
import scala.util.{Try, Using}

/**
 * Generator of input files for ClassSetFunctionRunner, for Maven projects in a given workspace directory. For each such project, a JSON
 * configuration file and class path file are generated. This program is unaware of the class function to be used, just like the
 * ClassSetFunctionRunner program itself is unaware of the specific class function used. So the class function JSON config file is out of
 * scope for this program.
 *
 * @author
 *   Chris de Vreeze
 */
object GenerateInputFilesForMavenProjects extends ManagedLogger {

  final case class Command(command: Seq[String])

  final case class Project(name: String, rootPackagePaths: Seq[String], targetClassesDir: Path = Path.of("target/classes"))

  final case class Config(
      workingDirectory: Path,
      mavenCommand: String = "mvn",
      projects: Seq[Project],
      outputDirectory: Path,
      gitBranch: String,
      initialCommands: Seq[Command]
  )

  final case class RunnerConfig(
      packagePaths: Set[Path], // Sub-packages will also be iterated over
      excludedPackagePaths: Set[Path],
      searchPathsWithinClassPath: Seq[Path] // May be partial, such as just a JAR file name without path
  )

  implicit val pathDecoder: Decoder[Path] = Decoder.decodeString.map(s => Path.of(s))
  implicit val commandDecoder: Decoder[Command] = deriveDecoder[Command]
  implicit val projectDecoder: Decoder[Project] = deriveDecoder[Project]
  implicit val configDecoder: Decoder[Config] = deriveDecoder[Config]

  implicit val pathEncoder: Encoder[Path] = Encoder.encodeString.contramap(_.toString)
  implicit val runnerConfigEncoder: Encoder[RunnerConfig] = deriveEncoder[RunnerConfig]

  def main(args: Array[String]): Unit = {
    require(
      args.sizeIs == 1,
      s"Usage: GenerateInputFilesForMavenProjects <JSON config resource path>"
    )
    val configJsonPath: String = args(0)

    val componentSupplier: ComponentSupplier = ComponentContainer.getInstance()
    val pathHelper: PathHelper = componentSupplier.getPathHelper()

    val configFile: Path = getFilePath(configJsonPath, pathHelper)

    val config: Config =
      parseJson(configFile)
        .flatMap(_.as[Config].toOption)
        .getOrElse(sys.error("Could not interpret the JSON program parameter as Config"))

    runForMaven(config)
  }

  def runForMaven(config: Config): Unit = {
    implicit val workingDirectory: Path = config.workingDirectory

    runCommand(Seq("mkdir", "-p", config.outputDirectory.toAbsolutePath.toString))
      .ensuring(_ == 0, s"Could not create output directory ${config.outputDirectory}")

    config.initialCommands.foreach { cmd =>
      runCommand(cmd.command).ensuring(_ == 0, s"Unsuccessful (initial) command: ${cmd.command}")
    }

    config.projects.foreach { project =>
      val projectPath: Path = config.workingDirectory.resolve(project.name)

      if (isMavenProject(projectPath)) {
        runForMavenProject(project, config)
      }
    }
  }

  private def isMavenProject(path: Path): Boolean = {
    val depth = 1
    Files.isDirectory(path) &&
    Files.find(path, depth, { (p, _) => p.getFileName == Path.of("pom.xml") }).findFirst().isPresent
  }

  private def runForMavenProject(project: Project, config: Config): Unit = {
    val projectOption: Option[Project] = config.projects.find(_.name == project.name)

    projectOption.foreach { project =>
      Try {
        implicit val projectPath: Path = config.workingDirectory.resolve(project.name)
        val projectName = project.name

        runCommand(Seq("echo", s"'Running for project $projectName'"))
        runCommand(Seq("git", "checkout", config.gitBranch))
          .ensuring(_ == 0, s"Could not checkout branch '${config.gitBranch}'")
        runCommand(Seq("git", "pull"))
          .ensuring(_ == 0, s"Could not pull project $projectName")
        runCommand(Seq(config.mavenCommand, "clean", "compile"))
          .ensuring(_ == 0, s"Could not compile project $projectName")

        runCommand(Seq(config.mavenCommand, "dependency:build-classpath"))
          .ensuring(_ == 0, s"Could not build class path string for project $projectName")

        val logPatterns = Set("INFO", "DEBUG", "WARN", "WARNING", "ERROR") // Brittle

        // One more time, to get just the class path string
        val rawCp: String = Using
          .resource(new ByteArrayOutputStream()) { bos =>
            runCommand(Seq(config.mavenCommand, "dependency:build-classpath"), bos)
              .ensuring(_ == 0, s"Could not build class path string for project $projectName")
            bos.toString(StandardCharsets.UTF_8)
          }
          .pipe(s => Source.fromString(s))
          .getLines()
          .toSeq
          .filterNot(line => logPatterns.exists(patt => line.contains(patt)))
          .headOption
          .getOrElse(sys.error(s"Could not find output line with class path string"))
          .trim

        val cp: Seq[String] = rawCp
          .split(':')
          .toSeq
          .map(_.trim)
          .prepended(projectPath.resolve(project.targetClassesDir).toAbsolutePath.toString)

        val cpFile: Path =
          config.outputDirectory.toAbsolutePath.resolve(s"$projectName-cp.txt")
        Files.deleteIfExists(cpFile)
        Files.createFile(cpFile)
        Files.writeString(cpFile, cp.mkString("\n"))

        val runnerConfig: RunnerConfig = RunnerConfig(
          packagePaths = project.rootPackagePaths.map(s => Path.of(s)).toSet,
          excludedPackagePaths = Set.empty,
          searchPathsWithinClassPath = Seq(projectPath.resolve(project.targetClassesDir))
        )

        val runnerConfigFile: Path =
          config.outputDirectory.toAbsolutePath.resolve(s"$projectName-config.json")
        Files.deleteIfExists(runnerConfigFile)
        Files.createFile(runnerConfigFile)
        Files.writeString(runnerConfigFile, Printer.indented("  ").print(runnerConfig.asJson))
      }.fold(
        t => System.err.println(s"Project $project. Exception thrown: $t"),
        identity
      )
    }
  }

  private def runCommand(cmd: Seq[String])(implicit workingDirectory: Path): Int = {
    runCommand(cmd, System.out)
  }

  private def runCommand(cmd: Seq[String], os: OutputStream)(implicit workingDirectory: Path): Int = {
    // See https://www.baeldung.com/java-lang-processbuilder-api
    val processBuilder: ProcessBuilder = new ProcessBuilder(cmd: _*)
      .directory(workingDirectory.toFile)

    val process: Process = processBuilder.start()
    val processHandle: ProcessHandle = process.toHandle()
    val processInfo: ProcessHandle.Info = processHandle.info()

    logInfo("Info: " + processInfo)
    logInfo("PID: " + processHandle.pid())
    logInfo("Alive: " + processHandle.isAlive)

    Using.resource(process.getInputStream()) { is =>
      is.transferTo(os)
    }

    val exitValue = process.waitFor()
    exitValue
  }

  private def getFilePath(file: String, pathHelper: PathHelper): Path = {
    if (Path.of(file).isAbsolute) Path.of(file)
    else
      pathHelper
        .getResource(file)
        .pipe(_.getAbsolutePath)
        .pipe(path => Path.of(path))
  }

  private def parseJson(file: Path): Option[Json] = {
    file
      .pipe(path => Files.readString(path))
      .pipe(parser.parse)
      .toOption
  }
}
