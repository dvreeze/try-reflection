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
import io.circe.{Decoder, Json}
import io.circe.generic.semiauto.*

/**
 * Class function searching for usage of the given types, in class/interface names, names of superclass or implemented interfaces, method
 * signatures, constructor signatures, field types, etc.
 *
 * @author
 *   Chris de Vreeze
 */
final class FindUsagesOfTypes(val classesToFind: Seq[Class[_]]) extends ClassFunctionReturningJson:

  def apply(clazz: Class[_]): Json =
    val results: Seq[Json] =
      for {
        classToFind <- classesToFind
      } yield findClass(classToFind, clazz)
    Json.fromValues(results)

  private def findClass(classToFind: Class[_], classToInspect: Class[_]): Json =
    val superclassOption = Option(classToInspect.getSuperclass())
    val interfaces = classToInspect.getInterfaces().toSeq
    val constructors = classToInspect.getDeclaredConstructors()
    val fields = classToInspect.getDeclaredFields().toSeq
    val methods = classToInspect.getDeclaredMethods().toSeq

    val classMatches = areEqual(classToFind, classToInspect)
    val superclassMatches = superclassOption.exists(cls => areEqual(classToFind, cls))
    val matchingInterfaces = interfaces.filter(itf => areEqual(classToFind, itf))
    val matchingConstructors = constructors.filter(_.getParameterTypes().exists(c => areEqual(classToFind, c)))
    val matchingFields = fields.filter(fld => areEqual(classToFind, fld.getType()))
    val matchingMethods = methods.filter { method =>
      method.getParameterTypes().exists(c => areEqual(classToFind, c)) ||
      areEqual(classToFind, method.getReturnType())
    }

    if classMatches || superclassMatches || matchingInterfaces.nonEmpty ||
      matchingConstructors.nonEmpty || matchingFields.nonEmpty || matchingMethods.nonEmpty
    then
      Json.obj(
        "inspectedClass" -> Json.fromString(classToInspect.getTypeName),
        "classToFind" -> Json.fromString(classToFind.getTypeName),
        "classNameMatches" -> Json.fromBoolean(classMatches),
        "superclassMatches" -> Json.fromBoolean(superclassMatches),
        "matchingInterfaces" -> Json.arr(matchingInterfaces.map(c => Json.fromString(c.toString)): _*),
        "matchingConstructors" -> Json.fromBoolean(matchingConstructors.nonEmpty),
        "matchingFields" -> Json.arr(matchingFields.map(f => Json.fromString(f.getName)): _*),
        "matchingMethods" -> Json.arr(matchingMethods.map(m => Json.fromString(m.getName)): _*)
      )
    else
      Json.obj(
        "inspectedClass" -> Json.fromString(classToInspect.getTypeName),
        "classToFind" -> Json.fromString(classToFind.getTypeName)
      )
  end findClass

  private def areEqual(class1: Class[_], class2: Class[_]): Boolean =
    class1.isAssignableFrom(class2) && class2.isAssignableFrom(class1)

object FindUsagesOfTypes extends ClassFunctionFactory[Json, FindUsagesOfTypes]:

  final case class Config(classesToFind: Seq[String])

  private given Decoder[Config] = deriveDecoder[Config]

  def create(configJson: Json): FindUsagesOfTypes =
    val config: Config = configJson.as[Config].toOption.get
    val classesToFind: Seq[Class[_]] = config.classesToFind.map(Class.forName)
    FindUsagesOfTypes(classesToFind)

end FindUsagesOfTypes
