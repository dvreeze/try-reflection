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

import scala.util.Try

/**
 * Class function searching for usage of the given types, in class/interface names, names of superclass or implemented interfaces, method
 * signatures, constructor signatures, field types, etc.
 *
 * @author
 *   Chris de Vreeze
 */
final class FindUsagesOfTypes(val classesToFind: Seq[Class[_]]) extends ClassFunctionReturningJson:

  // TODO One JSON per inspected class (skipping it if no usages of given types found)
  // TODO Find also inherited types of given types
  // TODO Search only relevant part of classpath (requires extra global config field?)

  def apply(clazz: Class[_]): Json =
    val results: Seq[Json] =
      (for {
        classToFind <- classesToFind
      } yield findClass(classToFind, clazz)).flatten

    if results.isEmpty then Json.arr(Json.obj("inspectedClass" -> Json.fromString(clazz.getTypeName)))
    else Json.fromValues(results)

  private def findClass(classToFind: Class[_], classToInspect: Class[_]): Option[Json] =
    Try {
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

      val classAnnotations = classToInspect.getDeclaredAnnotations().toSeq
      val constructorAnnotations = constructors.flatMap(_.getDeclaredAnnotations().toSeq)
      val fieldAnnotations = fields.flatMap(_.getDeclaredAnnotations().toSeq)
      val methodAnnotations = methods.flatMap(_.getDeclaredAnnotations().toSeq)

      val matchingClassAnnotations = classAnnotations.filter(a => areEqual(classToFind, a.annotationType()))
      val matchingConstructorAnnotations = constructorAnnotations.filter(a => areEqual(classToFind, a.annotationType()))
      val matchingFieldAnnotations = fieldAnnotations.filter(a => areEqual(classToFind, a.annotationType()))
      val matchingMethodAnnotations = methodAnnotations.filter(a => areEqual(classToFind, a.annotationType()))

      if classMatches || superclassMatches || matchingInterfaces.nonEmpty ||
        matchingConstructors.nonEmpty || matchingFields.nonEmpty || matchingMethods.nonEmpty ||
        matchingClassAnnotations.nonEmpty || matchingConstructorAnnotations.nonEmpty ||
        matchingFieldAnnotations.nonEmpty || matchingMethodAnnotations.nonEmpty
      then
        Some(
          Json.obj(
            "inspectedClass" -> Json.fromString(classToInspect.getTypeName),
            "classToFind" -> Json.fromString(classToFind.getTypeName),
            "superclass" -> superclassOption.map(c => Json.fromString(c.getTypeName)).getOrElse(Json.Null),
            "classNameMatches" -> Json.fromBoolean(classMatches),
            "superclassMatches" -> Json.fromBoolean(superclassMatches),
            "matchingInterfaces" -> Json.arr(matchingInterfaces.map(c => Json.fromString(c.toString)): _*),
            "matchingConstructors" -> Json.fromBoolean(matchingConstructors.nonEmpty),
            "matchingFields" -> Json.arr(matchingFields.map(f => Json.fromString(f.getName)): _*),
            "matchingMethods" -> Json.arr(matchingMethods.map(m => Json.fromString(m.getName)): _*),
            "matchingClassAnnotations" -> Json.arr(matchingClassAnnotations.map(a => Json.fromString(a.toString)): _*),
            "matchingConstructorAnnotations" -> Json.arr(matchingConstructorAnnotations.map(a => Json.fromString(a.toString)): _*),
            "matchingFieldAnnotations" -> Json.arr(matchingFieldAnnotations.map(a => Json.fromString(a.toString)): _*),
            "matchingMethodAnnotations" -> Json.arr(matchingMethodAnnotations.map(a => Json.fromString(a.toString)): _*)
          )
        )
      else None
    }.recover { case t: Throwable =>
      Some(
        Json.obj(
          "exceptionThrown" -> Json.fromString(t.toString)
        )
      )
    }.toOption
      .get
  end findClass

  private def areEqual(class1: Class[_], class2: Class[_]): Boolean =
    class1.isAssignableFrom(class2) && class2.isAssignableFrom(class1)

object FindUsagesOfTypes extends ClassFunctionFactory[Json, FindUsagesOfTypes]:

  final case class Config(classesToFind: Seq[String])

  private given Decoder[Config] = deriveDecoder[Config]

  def create(configJson: Json): FindUsagesOfTypes =
    val config: Config = configJson.as[Config].toOption.get
    val classesToFind: Seq[Class[_]] = config.classesToFind.flatMap(c => Try(Class.forName(c)).toOption)
    FindUsagesOfTypes(classesToFind)

end FindUsagesOfTypes
