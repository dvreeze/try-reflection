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

import eu.cdevreeze.tryreflection.introspection.{ClassFunctionFactory, ClassFunctionReturningJson}
import io.circe.{Decoder, Encoder, Json, parser}
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder
import io.circe.parser.parse
import org.burningwave.core.assembler.{ComponentContainer, ComponentSupplier}
import org.burningwave.core.classes.{ClassCriteria, ClassHunter, PathScannerClassLoader, SearchConfig}
import org.burningwave.core.io.PathHelper

import java.nio.file.{Files, Path}
import java.util.Properties
import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.util.Using
import scala.util.chaining.scalaUtilChainingOps

/**
 * ClassFunction runner.
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

  final case class ConfigResolver(config: Config):
    def resolveClassFunctionFactoryClass(classHunter: ClassHunter): Class[ClassFunctionFactory[Json, _ <: ClassFunctionReturningJson]] =
      val searchConfig = SearchConfig.byCriteria(
        ClassCriteria.create().className(_ == config.classFunctionFactoryClass)
      )
      Using.resource(classHunter.findBy(searchConfig)) { searchResult =>
        searchResult.getClasses
          .ensuring(_.size == 1, s"Expected exactly 1 '${config.classFunctionFactoryClass}' but got ${searchResult.getClasses.size} ones")
          .stream()
          .findFirst()
          .ensuring(_.isPresent)
          .get()
          .asInstanceOf[Class[ClassFunctionFactory[Json, _ <: ClassFunctionReturningJson]]]
      }

  def main(args: Array[String]): Unit =
    val configJsonPath: String =
      args.ensuring(_.nonEmpty, s"Usage: SimpleClassFunctionRunner <JSON file resource path> (e.g. sample-GetSupertypes-config.json)").head

    val defaultComponentSupplier: ComponentSupplier = ComponentContainer.getInstance()
    val defaultPathHelper: PathHelper = defaultComponentSupplier.getPathHelper()

    val config: Config =
      defaultPathHelper
        .getResource(configJsonPath)
        .pipe(_.getAbsolutePath)
        .pipe(path => Path.of(path))
        .pipe(path => Files.readString(path))
        .pipe(parser.parse)
        .toOption
        .flatMap(_.as[Config].toOption)
        .getOrElse(sys.error(s"Could not interpret the JSON program input as Config"))
    val configResolver = ConfigResolver(config)

    // The classpath of the code to inspect, which is "added" to the main classpath
    val additionalClassPath: Seq[String] = config.additionalClassPath.filter(_.trim.nonEmpty).ensuring(_.nonEmpty)

    val additionalClassPathsKeyAsPath = "java-memory-compiler.additional-class-paths"
    val additionalClassPathsKey = PathHelper.Configuration.Key.PATHS_PREFIX + additionalClassPathsKeyAsPath

    // We are now going to create a new ComponentContainer that uses this additional classpath

    val configProps = new Properties()
    configProps.put(additionalClassPathsKey, additionalClassPath.mkString(";")) // Colon does not work
    val componentSupplier: ComponentSupplier = ComponentContainer.create(configProps)

    val pathScannerClassLoader: PathScannerClassLoader = componentSupplier.getPathScannerClassLoader()
    val pathHelper: PathHelper = componentSupplier.getPathHelper()
    val classHunter: ClassHunter = componentSupplier.getClassHunter()

    assert(pathHelper.getPaths(additionalClassPathsKeyAsPath).stream().findFirst().isPresent())

    val searchConfigForInputClasses = SearchConfig
      .forPaths(
        pathHelper.getPaths(additionalClassPathsKeyAsPath) // Specifically and only searching here!
      )
      .by(
        ClassCriteria.create().className(cls => config.inputClassNames.contains(cls))
      )

    val jsonResult: Json =
      Using.resource(classHunter.findBy(searchConfigForInputClasses)) { searchResult =>
        val factoryClass = configResolver.resolveClassFunctionFactoryClass(classHunter)

        val factoryInput: Json = config.classFunctionFactoryJsonInput

        val factory: ClassFunctionFactory[Json, ClassFunctionReturningJson] =
          factoryClass
            .getField("MODULE$") // Assuming a singleton/companion object
            .get(null)
            .asInstanceOf[ClassFunctionFactory[Json, ClassFunctionReturningJson]]

        val classFunction: ClassFunctionReturningJson = factory.create(factoryInput)

        val clazzes: Seq[Class[_]] = searchResult.getClasses.asScala.toList
        Json.fromValues(clazzes.flatMap(cls => Try(classFunction(cls)).toOption))
      }

    println(jsonResult)
  end main

end SimpleClassFunctionRunner
