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

import eu.cdevreeze.tryreflection.introspection.{ClassSetFunctionFactory, ClassSetFunctionReturningJson}
import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.burningwave.core.classes.{ClassCriteria, ClassHunter, SearchConfig}

import scala.util.Using

/**
 * ClassSetFunction factories, instantiated from JSON configuration.
 *
 * @author
 *   Chris de Vreeze
 */
object ClassSetFunctionFactories:

  final case class Config(
      name: String,
      classSetFunctionFactoryClass: String,
      classSetFunctionFactoryJsonInput: Json
  )

  given Encoder[Config] = deriveEncoder[Config]

  given Decoder[Config] = deriveDecoder[Config]

  final case class ConfigResolver(config: Config):

    def resolveClassSetFunctionFactoryClass(
        classHunter: ClassHunter
    ): Class[ClassSetFunctionFactory[Json, ? <: ClassSetFunctionReturningJson]] =
      val searchConfig = SearchConfig.byCriteria(
        ClassCriteria.create().className(_ == config.classSetFunctionFactoryClass)
      )
      Using.resource(classHunter.findBy(searchConfig)) { searchResult =>
        searchResult.getClasses
          .ensuring(
            _.size == 1,
            s"Expected exactly 1 '${config.classSetFunctionFactoryClass}' but got ${searchResult.getClasses.size} ones"
          )
          .stream()
          .findFirst()
          .ensuring(_.isPresent)
          .get()
          .asInstanceOf[Class[ClassSetFunctionFactory[Json, ? <: ClassSetFunctionReturningJson]]]
      }
    end resolveClassSetFunctionFactoryClass

    def resolveClassSetFunctionFactory(classHunter: ClassHunter): ClassSetFunctionFactory[Json, ? <: ClassSetFunctionReturningJson] =
      val factoryClass = resolveClassSetFunctionFactoryClass(classHunter)

      val factory: ClassSetFunctionFactory[Json, ClassSetFunctionReturningJson] =
        factoryClass
          .getField("MODULE$") // Assuming a singleton/companion object
          .get(null)
          .asInstanceOf[ClassSetFunctionFactory[Json, ClassSetFunctionReturningJson]]

      factory
    end resolveClassSetFunctionFactory

    def resolveClassSetFunction(classHunter: ClassHunter): ClassSetFunctionReturningJson =
      val factory = resolveClassSetFunctionFactory(classHunter)

      val factoryInput: Json = config.classSetFunctionFactoryJsonInput

      val classFunction: ClassSetFunctionReturningJson = factory.create(factoryInput)
      classFunction
    end resolveClassSetFunction

  end ConfigResolver

end ClassSetFunctionFactories
