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

import io.circe.{Decoder, Encoder, Json, parser}
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder
import io.circe.parser.parse
import org.burningwave.core.ManagedLogger
import org.burningwave.core.assembler.{ComponentContainer, ComponentSupplier}
import org.burningwave.core.io.PathHelper

import java.nio.file.{Files, Path}
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.chaining.scalaUtilChainingOps

/**
 * ClassFunction runner. It takes a JSON config file, as well as a class path file.
 *
 * It forks program InternalSimpleClassFunctionRunner in a different OS process, enhancing its classpath.
 *
 * The class path file could come from running command "mvn dependency:build-classpath", or command "cs fetch" on some artifact. Do not
 * forget to then filter out the Scala library etc.
 *
 * @author
 *   Chris de Vreeze
 */
object SimpleClassFunctionRunner extends ManagedLogger:

  final case class Config(
      name: String,
      classFunctionFactoryClass: String,
      classFunctionFactoryJsonInput: Json,
      inputClassNames: Seq[String]
  )

  private given Encoder[Config] = deriveEncoder[Config]
  private given Decoder[Config] = deriveDecoder[Config]

  def main(args: Array[String]): Unit =
    require(
      args.sizeIs == 2,
      s"Usage: SimpleClassFunctionRunner <JSON file resource path> <classpath file> "
    )
    val configJsonPath: String = args(0)
    val classPathFilePath: String = args(1)

    val componentSupplier: ComponentSupplier = ComponentContainer.getInstance()
    val pathHelper: PathHelper = componentSupplier.getPathHelper()

    val configFile: Path =
      pathHelper
        .getResource(configJsonPath)
        .pipe(_.getAbsolutePath)
        .pipe(path => Path.of(path))

    val configOption: Option[Config] =
      configFile
        .pipe(path => Files.readString(path))
        .pipe(parser.parse)
        .toOption
        .flatMap(_.as[Config].toOption)
    require(configOption.nonEmpty, "Could not interpret the JSON program input as Config")

    val classPathFile: Path =
      pathHelper
        .getResource(classPathFilePath)
        .pipe(_.getAbsolutePath)
        .pipe(path => Path.of(path))

    val totalClassPath: String = getCombinedClassPath(classPathFile)

    val cpFile: Path = Files.createTempFile("classpath-", ".txt")
    Files.writeString(cpFile, s"-cp $totalClassPath")

    val mainClassName =
      classOf[SimpleClassFunctionRunner.type].getPackageName
        + ".internal.InternalSimpleClassFunctionRunner"

    // See https://docs.oracle.com/javase/9/tools/java.htm#JSWOR-GUID-4856361B-8BFD-4964-AE84-121F5F6CF111
    val javaCommand: Seq[String] =
      Seq("java", s"@${cpFile.toAbsolutePath}", mainClassName, configFile.toAbsolutePath.toString)

    // See https://www.baeldung.com/java-lang-processbuilder-api
    val processBuilder: ProcessBuilder = new ProcessBuilder(javaCommand: _*)

    val process: Process = processBuilder.start()
    val processHandle: ProcessHandle = process.toHandle()
    val processInfo: ProcessHandle.Info = processHandle.info()

    logInfo("PID: " + processHandle.pid())
    logInfo("Alive: " + processHandle.isAlive)
    logInfo("Info: " + processInfo)

    Using.resource(process.getInputStream()) { is =>
      is.transferTo(System.out)
    }

    val exitValue = process.waitFor()
    require(exitValue == 0, s"Expected exit value 0, but got $exitValue")
  end main

  private def getCombinedClassPath(classPathFile: Path): String =
    val ownClassPath: Seq[String] = System
      .getProperty("java.class.path")
      .ensuring(!_.contains(";"))
      .pipe(_.split(':').toSeq)

    val extraClassPath: Seq[String] = Source
      .fromFile(classPathFile.toFile)
      .getLines()
      .toSeq
      .ensuring(_.forall(!_.contains(";")))
      .flatMap(_.split(':').toSeq)

    // TODO Deduplicate
    ownClassPath.mkString(":") + ":" + extraClassPath.mkString(":")
  end getCombinedClassPath

end SimpleClassFunctionRunner
