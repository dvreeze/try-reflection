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

package eu.cdevreeze.tryreflection.introspection.classfunctions

import eu.cdevreeze.tryreflection.introspection.{ClassFunctionFactory, ClassFunctionReturningJson}
import io.circe.Json

import java.lang.reflect.{ParameterizedType, Type}

/**
 * Class function that finds the super-classes and extended interfaces of a class.
 *
 * @author
 *   Chris de Vreeze
 */
final class GetSupertypes() extends ClassFunctionReturningJson:

  def apply(clazz: Class[?]): Json =
    val superclasses: Seq[Type] = findAllSuperclasses(clazz)
    val interfaces: Seq[Type] =
      superclasses.prepended(clazz).flatMap(t => toClassOption(t)).flatMap(findAllInterfaces).distinct

    Json.obj(
      "class" ->
        Json.fromString(clazz.getTypeName),
      "superclasses" ->
        Json.fromValues(superclasses.map(_.toString).map(Json.fromString)),
      "extendedInterfaces" ->
        Json.fromValues(interfaces.map(_.toString).map(Json.fromString))
    )
  end apply

  private def findAllSuperclasses(clazz: Class[?]): Seq[Type] =
    findAllSupertypes(clazz).filter(cls => toClassOption(cls).exists(!_.isInterface))

  private def findAllInterfaces(clazz: Class[?]): Seq[Type] =
    findAllSupertypes(clazz).filter(cls => toClassOption(cls).exists(_.isInterface))

  private def findAllSupertypes(clazz: Class[?]): Seq[Type] =
    val superclassOption: Option[Type] = Option(clazz.getGenericSuperclass())
    val interfaces: Seq[Type] = clazz.getGenericInterfaces().toSeq
    // Recursion
    superclassOption.toSeq.appendedAll(interfaces).flatMap(toClassOption).flatMap(findAllSupertypes)

  private def toClassOption(tpe: Type): Option[Class[?]] =
    tpe match
      case cls: Class[?]            => Option(cls)
      case ptype: ParameterizedType => Option(ptype.getRawType).collect { case cls: Class[?] => cls }
      case _                        => None

object GetSupertypes extends ClassFunctionFactory[Json, GetSupertypes]:

  def create(configJson: Json): GetSupertypes = GetSupertypes()

end GetSupertypes
