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

package eu.cdevreeze.tryreflection.introspection.rules

import eu.cdevreeze.tryreflection.introspection.SingleClassRule
import io.circe.Json

import java.lang.reflect.{ParameterizedType, Type}

/**
 * Rule that shows the super-classes and extended interfaces of a class.
 *
 * @author
 *   Chris de Vreeze
 */
object ShowSupertypes extends SingleClassRule:

  def introspect(clazz: Class[_]): Json =
    val superclasses: Seq[Type] = findSuperclasses(clazz)
    val interfaces: Seq[Type] =
      superclasses.prepended(clazz).flatMap(t => toClassOption(t)).flatMap(findInterfaces).distinct

    Json.obj(
      "class" ->
        Json.fromString(clazz.getTypeName),
      "superclasses" ->
        Json.fromValues(superclasses.map(_.toString).map(Json.fromString)),
      "extendedInterfaces" ->
        Json.fromValues(interfaces.map(_.toString).map(Json.fromString))
    )
  end introspect

  private def findSuperclasses(clazz: Class[_]): Seq[Type] =
    val superclassOption: Option[Type] = Option(clazz.getGenericSuperclass())
    // Recursion
    superclassOption
      .map(cls => toClassOption(cls).toSeq.flatMap(findSuperclasses).prepended(cls))
      .getOrElse(Seq.empty)

  private def findInterfaces(clazz: Class[_]): Seq[Type] =
    val interfaces: Seq[Type] = clazz.getGenericInterfaces().toSeq
    // Recursion
    interfaces.appendedAll(interfaces.flatMap(toClassOption).flatMap(findInterfaces)).distinct

  private def toClassOption(tpe: Type): Option[Class[_]] =
    tpe match
      case cls: Class[_]            => Option(cls)
      case ptype: ParameterizedType => Option(ptype.getRawType).collect { case cls: Class[_] => cls }
      case _                        => None

end ShowSupertypes
