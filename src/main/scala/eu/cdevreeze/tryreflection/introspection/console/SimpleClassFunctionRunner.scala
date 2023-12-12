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
import org.burningwave.core.assembler.{ComponentContainer, ComponentSupplier}
import org.burningwave.core.io.PathHelper

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.chaining.scalaUtilChainingOps

/**
 * ClassFunction runner.
 *
 * It forks program InternalSimpleClassFunctionRunner in a different OS process, enhancing its classpath.
 *
 * @author
 *   Chris de Vreeze
 */
object SimpleClassFunctionRunner:

  final case class Config(
      name: String,
      classFunctionFactoryClass: String,
      classFunctionFactoryJsonInput: Json,
      additionalClassPath: Seq[String], // E.g., created with "mvn dependency:build-classpath"
      inputClassNames: Seq[String]
  )

  private given Encoder[Config] = deriveEncoder[Config]
  private given Decoder[Config] = deriveDecoder[Config]

  def main(args: Array[String]): Unit =
    val configJsonPath: String =
      args.ensuring(_.nonEmpty, s"Usage: SimpleClassFunctionRunner <JSON file resource path> (e.g. sample-GetSupertypes-config.json)").head

    val componentSupplier: ComponentSupplier = ComponentContainer.getInstance()
    val pathHelper: PathHelper = componentSupplier.getPathHelper()

    val config: Config =
      pathHelper
        .getResource(configJsonPath)
        .pipe(_.getAbsolutePath)
        .pipe(path => Path.of(path))
        .pipe(path => Files.readString(path))
        .pipe(parser.parse)
        .toOption
        .flatMap(_.as[Config].toOption)
        .getOrElse(sys.error(s"Could not interpret the JSON program input as Config"))

    // See https://www.baeldung.com/java-lang-processbuilder-api and https://www.baeldung.com/java-9-process-api

    // Using pipelines in the ProcessBuilder API:
    // Call something like "mvn dependency:build-classpath"
    // Remove unwanted lines
    // Call "java -cp <cp> InternalSimpleClassFunctionRunner ..." (that program must be found)
    // Wait for the pipeline to finish, and return its output

    // If mvn does not exist as a command but mvnw is used instead, just create an alias before running this program.
    // Of course we could also run "cs fetch --classpath ..." instead if no POM file is encountered in the current directory.

    // For "sed", see https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/.
    // For "awk", see https://www.geeksforgeeks.org/awk-command-unixlinux-examples/.

    // For running the "java" command with argument files, see https://docs.oracle.com/en/java/javase/17/docs/specs/man/java.html#java-command-line-argument-files.
    ???
  end main

end SimpleClassFunctionRunner
