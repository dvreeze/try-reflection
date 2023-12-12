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

package eu.cdevreeze.tryreflection.introspection.console.internal

import eu.cdevreeze.tryreflection.introspection.{ClassFunctionFactory, ClassFunctionReturningJson}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder, Json, parser}
import org.burningwave.core.assembler.{ComponentContainer, ComponentSupplier}
import org.burningwave.core.classes.{ClassCriteria, ClassHunter, JavaClass, SearchConfig}
import org.burningwave.core.io.{FileSystemItem, PathHelper}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}
import scala.util.chaining.scalaUtilChainingOps

/**
 * Internal ClassFunctionRunnerIteratingOverClassPath runner, called as program in a different OS process from
 * ClassFunctionRunnerIteratingOverClassPath. The calling program makes sure this program runs with the correct classpath. Having the
 * correct classpath, this program does all the work of running a ClassFunction on given parts of the classpath.
 *
 * @author
 *   Chris de Vreeze
 */
object InternalClassFunctionRunnerIteratingOverClassPath:

  final case class Config(
      name: String,
      classFunctionFactoryClass: String,
      classFunctionFactoryJsonInput: Json,
      additionalClassPath: Seq[String], // Ignored here
      packagePaths: Set[String], // Sub-packages will also be iterated over
      excludedPackagePaths: Set[String],
      searchPathsWithinClassPath: Seq[String] // May be partial, such as just a JAR file name without path
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
      args
        .ensuring(
          _.nonEmpty,
          s"Usage: InternalClassFunctionRunnerIteratingOverClassPath <JSON file resource path> (e.g. sample-GetSupertypes-config.json)"
        )
        .head

    val componentSupplier: ComponentSupplier = ComponentContainer.getInstance()
    val pathHelper: PathHelper = componentSupplier.getPathHelper()
    val classHunter: ClassHunter = componentSupplier.getClassHunter()

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
    val configResolver = ConfigResolver(config)

    val searchPaths: java.util.Collection[String] =
      if config.searchPathsWithinClassPath.isEmpty then pathHelper.getAllMainClassPaths
      else pathHelper.getPaths(p => config.searchPathsWithinClassPath.exists(cfgPath => p.contains(cfgPath)))

    val searchConfigForInputClasses = SearchConfig
      .forPaths(searchPaths)
      .addFileFilter(
        FileSystemItem.Criteria.forAllFileThat { fileSystemItem =>
          val javaClassOption = Option(fileSystemItem.toJavaClass).filterNot(skipClass)
          val javaClassPkgOption = javaClassOption.flatMap(c => Option(c.getPackagePath))

          val matches: Boolean =
            javaClassPkgOption.exists(p => config.packagePaths.exists(cfgPp => p.startsWith(cfgPp)))
              && javaClassPkgOption.forall(p => !config.excludedPackagePaths.exists(cfgPp => p.startsWith(cfgPp)))
          matches
        }
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

        def getOptionalFunctionResult(cls: Class[_]): Option[Json] =
          Try(classFunction(cls)).toOption.filterNot(_ == Json.obj())

        Json.fromValues(clazzes.flatMap(getOptionalFunctionResult))
      }

    println(jsonResult)
  end main

  private def skipClass(clazz: JavaClass): Boolean =
    val classNameWithoutExtension = clazz.getClassFileName.stripSuffix(".class")
    classNameWithoutExtension.stripSuffix("$").contains("$") ||
    classNameWithoutExtension.contains("package")

end InternalClassFunctionRunnerIteratingOverClassPath
