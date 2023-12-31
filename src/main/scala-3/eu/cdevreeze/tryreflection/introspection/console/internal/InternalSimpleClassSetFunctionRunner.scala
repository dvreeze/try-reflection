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

import eu.cdevreeze.tryreflection.introspection.console.ClassSetFunctionFactories
import eu.cdevreeze.tryreflection.introspection.ClassSetFunctionReturningJson
import io.circe.generic.semiauto.deriveDecoder
import io.circe.{Decoder, Json, parser}
import org.burningwave.core.assembler.{ComponentContainer, ComponentSupplier}
import org.burningwave.core.classes.{ClassCriteria, ClassHunter, JavaClass, SearchConfig}
import org.burningwave.core.io.PathHelper

import java.nio.file.{Files, Path}
import scala.collection.immutable.ListSet
import scala.jdk.CollectionConverters.*
import scala.util.chaining.scalaUtilChainingOps
import scala.util.{Try, Using}

/**
 * Internal ClassSetFunction runner, called as program in a different OS process from SimpleClassSetFunctionRunner. The calling program
 * makes sure this program runs with the correct classpath. Having the correct classpath, this program does all the work of running a
 * ClassSetFunction on given classes. The 2 program arguments are JSON config resource paths, one for the runner, and one for the
 * ClassSetFunction initialisation.
 *
 * @author
 *   Chris de Vreeze
 */
object InternalSimpleClassSetFunctionRunner:

  final case class Config(
      inputClassNames: Seq[String]
  )

  private given Decoder[Config] = deriveDecoder[Config]

  def main(args: Array[String]): Unit =
    val (configJsonPath, classSetFunctionConfigJsonPath) =
      args
        .ensuring(
          _.sizeIs == 2,
          s"Usage: InternalSimpleClassSetFunctionRunner <JSON config resource path> <ClassSetFunction JSON config resource path>"
        )
        .pipe(args => (args(0), args(1)))

    val componentSupplier: ComponentSupplier = ComponentContainer.getInstance()
    val pathHelper: PathHelper = componentSupplier.getPathHelper()
    val classHunter: ClassHunter = componentSupplier.getClassHunter()

    val configFile: Path = getFilePath(configJsonPath, pathHelper)

    val config: Config =
      parseJson(configFile)
        .flatMap(_.as[Config].toOption)
        .getOrElse(sys.error(s"Could not interpret the JSON program argument as Config"))

    val classSetFunctionConfigFile: Path = getFilePath(classSetFunctionConfigJsonPath, pathHelper)

    val classSetFunctionConfig: ClassSetFunctionFactories.Config =
      parseJson(classSetFunctionConfigFile)
        .flatMap(_.as[ClassSetFunctionFactories.Config].toOption)
        .getOrElse(sys.error(s"Could not interpret the JSON program argument as ClassSetFunctionFactories.Config"))

    val searchConfigForInputClasses = SearchConfig
      .forPaths(pathHelper.getAllMainClassPaths)
      .by(
        ClassCriteria.create().className { cls =>
          config.inputClassNames.contains(cls) &&
          !Try(skipClass(new JavaClass(Class.forName(cls)))).getOrElse(true)
        }
      )

    val jsonResult: Json =
      Using.resource(classHunter.findBy(searchConfigForInputClasses)) { searchResult =>
        val classSetFunctionConfigResolver = ClassSetFunctionFactories.ConfigResolver(classSetFunctionConfig)

        val classSetFunction: ClassSetFunctionReturningJson =
          classSetFunctionConfigResolver.resolveClassSetFunction(classHunter)

        val clazzes: Seq[Class[?]] = searchResult.getClasses.asScala.toList

        classSetFunction(clazzes.to(ListSet))
      }

    println(jsonResult)
  end main

  private def getFilePath(file: String, pathHelper: PathHelper): Path =
    if Path.of(file).isAbsolute then Path.of(file)
    else
      pathHelper
        .getResource(file)
        .pipe(_.getAbsolutePath)
        .pipe(path => Path.of(path))

  private def parseJson(file: Path): Option[Json] =
    file
      .pipe(path => Files.readString(path))
      .pipe(parser.parse)
      .toOption

  private def skipClass(clazz: JavaClass): Boolean =
    val classNameWithoutExtension = clazz.getClassFileName.stripSuffix(".class")
    classNameWithoutExtension.stripSuffix("$").contains("$") ||
    classNameWithoutExtension.contains("package")

end InternalSimpleClassSetFunctionRunner
