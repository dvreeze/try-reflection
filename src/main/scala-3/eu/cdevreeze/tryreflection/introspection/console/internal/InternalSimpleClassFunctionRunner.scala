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
import org.burningwave.core.io.PathHelper

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}
import scala.util.chaining.scalaUtilChainingOps

/**
 * Internal ClassFunction runner, called as program in a different OS process from SimpleClassFunctionRunner. The calling program makes sure
 * this program runs with the correct classpath. Having the correct classpath, this program does all the work of running a ClassFunction on
 * given classes.
 *
 * @author
 *   Chris de Vreeze
 */
object InternalSimpleClassFunctionRunner:

  final case class Config(
      name: String,
      classFunctionFactoryClass: String,
      classFunctionFactoryJsonInput: Json,
      inputClassNames: Seq[String]
  )

  private given Encoder[Config] = deriveEncoder[Config]

  private given Decoder[Config] = deriveDecoder[Config]

  final case class ConfigResolver(config: Config):
    def resolveClassFunctionFactoryClass(classHunter: ClassHunter): Class[ClassFunctionFactory[Json, ? <: ClassFunctionReturningJson]] =
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
          .asInstanceOf[Class[ClassFunctionFactory[Json, ? <: ClassFunctionReturningJson]]]
      }

  def main(args: Array[String]): Unit =
    val configJsonPath: String =
      args
        .ensuring(
          _.nonEmpty,
          s"Usage: InternalSimpleClassFunctionRunner <JSON file resource path> (e.g. sample-FindUsagesOfTypes-config.json)"
        )
        .head

    val componentSupplier: ComponentSupplier = ComponentContainer.getInstance()
    val pathHelper: PathHelper = componentSupplier.getPathHelper()
    val classHunter: ClassHunter = componentSupplier.getClassHunter()

    val configFile: Path =
      if Path.of(configJsonPath).isAbsolute then Path.of(configJsonPath)
      else
        pathHelper
          .getResource(configJsonPath)
          .pipe(_.getAbsolutePath)
          .pipe(path => Path.of(path))

    val config: Config =
      configFile
        .pipe(path => Files.readString(path))
        .pipe(parser.parse)
        .toOption
        .flatMap(_.as[Config].toOption)
        .getOrElse(sys.error(s"Could not interpret the JSON program input as Config"))
    val configResolver = ConfigResolver(config)

    val searchConfigForInputClasses = SearchConfig
      .forPaths(pathHelper.getAllMainClassPaths)
      .by(
        ClassCriteria.create().className { cls =>
          config.inputClassNames.contains(cls)
          && !Try(skipClass(new JavaClass(Class.forName(cls)))).getOrElse(true)
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

        val clazzes: Seq[Class[?]] = searchResult.getClasses.asScala.toList

        def getOptionalFunctionResult(cls: Class[?]): Option[Json] =
          Try(classFunction(cls)).toOption.filterNot(_ == Json.obj())

        Json.fromValues(clazzes.flatMap(getOptionalFunctionResult))
      }

    println(jsonResult)
  end main

  private def skipClass(clazz: JavaClass): Boolean =
    val classNameWithoutExtension = clazz.getClassFileName.stripSuffix(".class")
    classNameWithoutExtension.stripSuffix("$").contains("$") ||
    classNameWithoutExtension.contains("package")

end InternalSimpleClassFunctionRunner