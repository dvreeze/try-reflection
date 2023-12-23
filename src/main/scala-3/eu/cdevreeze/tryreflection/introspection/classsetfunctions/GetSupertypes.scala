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

package eu.cdevreeze.tryreflection.introspection.classsetfunctions

import eu.cdevreeze.tryreflection.internal.ReflectionUtils.*
import eu.cdevreeze.tryreflection.introspection.{ClassSetFunctionFactory, ClassSetFunctionReturningJson}
import io.circe.Json

import java.lang.reflect.Type
import scala.util.Try

/**
 * Class Set function that finds the super-classes and extended interfaces of a class.
 *
 * @author
 *   Chris de Vreeze
 */
final class GetSupertypes() extends ClassSetFunctionReturningJson:

  def apply(clazzes: Set[Class[?]]): Json =
    val resultsPerClass: Seq[Json] = clazzes.toSeq.flatMap { cls =>
      Try(applyForClass(cls)).toOption.filterNot(_ == Json.obj())
    }
    Json.fromValues(resultsPerClass)

  private def applyForClass(clazz: Class[?]): Json =
    val superclasses: Seq[Type] = findSuperclasses(clazz)
    val interfaces: Seq[Type] = findAllInterfaces(clazz)

    Json.obj(
      "class" ->
        Json.fromString(clazz.getTypeName),
      "superclasses" ->
        Json.fromValues(superclasses.map(_.toString).map(Json.fromString)),
      "extendedInterfaces" ->
        Json.fromValues(interfaces.map(_.toString).map(Json.fromString))
    )

object GetSupertypes extends ClassSetFunctionFactory[Json, GetSupertypes]:

  def create(configJson: Json): GetSupertypes = GetSupertypes()

end GetSupertypes
