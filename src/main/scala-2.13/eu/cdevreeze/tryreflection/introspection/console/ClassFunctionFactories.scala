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
import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.burningwave.core.classes.{ClassCriteria, ClassHunter, SearchConfig}

import scala.util.Using

/**
 * ClassFunction factories, instantiated from JSON configuration.
 *
 * @author
 *   Chris de Vreeze
 */
object ClassFunctionFactories {

  final case class Config(
      name: String,
      classFunctionFactoryClass: String,
      classFunctionFactoryJsonInput: Json
  )

  implicit val configEncoder: Encoder[Config] = deriveEncoder[Config]

  implicit val configDecoder: Decoder[Config] = deriveDecoder[Config]

  final case class ConfigResolver(config: Config) {

    def resolveClassFunctionFactoryClass(classHunter: ClassHunter): Class[ClassFunctionFactory[Json, _ <: ClassFunctionReturningJson]] = {
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
    }

    def resolveClassFunctionFactory(classHunter: ClassHunter): ClassFunctionFactory[Json, _ <: ClassFunctionReturningJson] = {
      val factoryClass = resolveClassFunctionFactoryClass(classHunter)

      val factory: ClassFunctionFactory[Json, ClassFunctionReturningJson] =
        factoryClass
          .getField("MODULE$") // Assuming a singleton/companion object
          .get(null)
          .asInstanceOf[ClassFunctionFactory[Json, ClassFunctionReturningJson]]

      factory
    }

    def resolveClassFunction(classHunter: ClassHunter): ClassFunctionReturningJson = {
      val factory = resolveClassFunctionFactory(classHunter)

      val factoryInput: Json = config.classFunctionFactoryJsonInput

      val classFunction: ClassFunctionReturningJson = factory.create(factoryInput)
      classFunction
    }
  }
}
