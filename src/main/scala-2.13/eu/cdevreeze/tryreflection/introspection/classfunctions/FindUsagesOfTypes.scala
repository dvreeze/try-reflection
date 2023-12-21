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

import eu.cdevreeze.tryreflection.internal.ReflectionUtils._
import eu.cdevreeze.tryreflection.introspection.{ClassFunctionFactory, ClassFunctionReturningJson}
import io.circe.generic.semiauto._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}

import java.lang.annotation.Annotation
import java.lang.reflect.{Constructor, Field, Method, Type}
import scala.util.Try

/**
 * Class function searching for usage of the given types, in class/interface names, names of superclass or implemented interfaces, method
 * signatures, constructor signatures, field types, etc.
 *
 * @author
 *   Chris de Vreeze
 */
final class FindUsagesOfTypes(val classesToFind: Seq[Class[_]]) extends ClassFunctionReturningJson {

  private implicit def constructorEncoder[A]: Encoder[Constructor[A]] = Encoder.encodeString.contramap(_.toGenericString)

  private implicit val fieldEncoder: Encoder[Field] = Encoder.encodeString.contramap(_.toGenericString)

  private implicit val methodEncoder: Encoder[Method] = Encoder.encodeString.contramap(_.toGenericString)

  private implicit val annotationEncoder: Encoder[Annotation] = Encoder.encodeString.contramap(_.toString)

  def apply(clazz: Class[_]): Json =
    findClasses(classesToFind, clazz).getOrElse(Json.obj())

  private def findClasses(classesToFind: Seq[Class[_]], classToInspect: Class[_]): Option[Json] = {
    val superclassOption: Option[Class[_]] = Option(classToInspect.getSuperclass())
    val interfaces = classToInspect.getInterfaces().toSeq
    val constructors = classToInspect.getDeclaredConstructors().toSeq

    val allGenericSuperclasses: Seq[Type] = findSuperclasses(classToInspect)
    val allGenericInterfaces: Seq[Type] = findAllInterfaces(classToInspect)

    val jsonsForFoundClasses: Seq[Json] = classesToFind.flatMap(cls => findClass(cls, classToInspect))
    val summaryJsonOption: Option[Json] = getSummary(classesToFind, classToInspect)

    if (jsonsForFoundClasses.isEmpty) None
    else
      Some(
        Json.obj(
          "inspectedClass" -> Json.fromString(classToInspect.getTypeName),
          "superclass" -> superclassOption.map(c => Json.fromString(c.getTypeName)).getOrElse(Json.Null),
          "interfaces" -> Json.arr(interfaces.map(c => Json.fromString(c.toString)): _*),
          "allGenericSuperclasses" -> Json.arr(allGenericSuperclasses.map(c => Json.fromString(c.toString)): _*),
          "allGenericInterfaces" -> Json.arr(allGenericInterfaces.map(c => Json.fromString(c.toString)): _*),
          "constructors" -> Json.arr(constructors.map(_.asJson): _*),
          "foundTypeUsages" -> Json.fromValues(jsonsForFoundClasses),
          "summary" -> summaryJsonOption.getOrElse(Json.obj())
        )
      )
  }

  private def findClass(classToFind: Class[_], classToInspect: Class[_]): Option[Json] = {
    Try {
      val superclassOption: Option[Class[_]] = Option(classToInspect.getSuperclass())
      val interfaces = classToInspect.getInterfaces().toSeq
      val constructors = classToInspect.getDeclaredConstructors()
      val fields = classToInspect.getDeclaredFields().toSeq
      val methods = classToInspect.getDeclaredMethods().toSeq

      val classMatches = classToFind.isAssignableFrom(classToInspect)
      val superclassMatches = superclassOption.exists(cls => classToFind.isAssignableFrom(cls))
      val matchingInterfaces = interfaces.filter(itf => classToFind.isAssignableFrom(itf))
      val matchingConstructors = constructors.filter(_.getParameterTypes().exists(c => classToFind.isAssignableFrom(c)))
      val matchingFields = fields.filter(fld => classToFind.isAssignableFrom(fld.getType()))
      val matchingMethods = methods.filter { method =>
        method.getParameterTypes().exists(c => classToFind.isAssignableFrom(c)) ||
        classToFind.isAssignableFrom(method.getReturnType())
      }

      val classAnnotations = classToInspect.getDeclaredAnnotations().toSeq
      val constructorAnnotations = constructors.flatMap(_.getDeclaredAnnotations()).toSeq
      val fieldAnnotations = fields.flatMap(_.getDeclaredAnnotations())
      val methodAnnotations = methods.flatMap(_.getDeclaredAnnotations())

      val matchingClassAnnotations = classAnnotations.filter(a => classToFind.isAssignableFrom(a.annotationType()))
      val matchingConstructorAnnotations = constructorAnnotations.filter(a => classToFind.isAssignableFrom(a.annotationType()))
      val matchingFieldAnnotations = fieldAnnotations.filter(a => classToFind.isAssignableFrom(a.annotationType()))
      val matchingMethodAnnotations = methodAnnotations.filter(a => classToFind.isAssignableFrom(a.annotationType()))

      if (
        classMatches || superclassMatches || matchingInterfaces.nonEmpty ||
        matchingConstructors.nonEmpty || matchingFields.nonEmpty || matchingMethods.nonEmpty ||
        matchingClassAnnotations.nonEmpty || matchingConstructorAnnotations.nonEmpty ||
        matchingFieldAnnotations.nonEmpty || matchingMethodAnnotations.nonEmpty
      ) {
        Some(
          Json.obj(
            "classToFind" -> Json.fromString(classToFind.getTypeName),
            "classNameMatches" -> Json.fromBoolean(classMatches),
            "superclassMatches" -> Json.fromBoolean(superclassMatches),
            "matchingInterfaces" -> Json.arr(matchingInterfaces.map(c => Json.fromString(c.toString)): _*),
            "matchingConstructors" -> Json.fromBoolean(matchingConstructors.nonEmpty),
            "matchingFields" -> Json.arr(matchingFields.map(_.asJson): _*),
            "matchingMethods" -> Json.arr(matchingMethods.map(_.asJson): _*),
            "matchingClassAnnotations" -> Json.arr(matchingClassAnnotations.map(_.asJson): _*),
            "matchingConstructorAnnotations" -> Json.arr(matchingConstructorAnnotations.map(_.asJson): _*),
            "matchingFieldAnnotations" -> Json.arr(matchingFieldAnnotations.map(_.asJson): _*),
            "matchingMethodAnnotations" -> Json.arr(matchingMethodAnnotations.map(_.asJson): _*)
          )
        )
      } else {
        None
      }
    }.recover { case t: Throwable =>
      Some(
        Json.obj(
          "exceptionThrown" -> Json.fromString(t.toString)
        )
      )
    }.toOption
      .get
  }

  private def getSummary(classesToFind: Seq[Class[_]], classToInspect: Class[_]): Option[Json] = {
    Try {
      val constructors = classToInspect.getDeclaredConstructors()
      val fields = classToInspect.getDeclaredFields().toSeq
      val methods = classToInspect.getDeclaredMethods().toSeq

      val classAnnotations = classToInspect.getDeclaredAnnotations().toSeq
      val constructorAnnotations = constructors.flatMap(_.getDeclaredAnnotations().toSeq)
      val fieldAnnotations = fields.flatMap(_.getDeclaredAnnotations().toSeq)
      val methodAnnotations = methods.flatMap(_.getDeclaredAnnotations().toSeq)

      val isAMatches = classesToFind.filter { cls =>
        cls.isAssignableFrom(classToInspect) ||
        classAnnotations.exists(a => cls.isAssignableFrom(a.annotationType()))
      }

      val hasAMatches = classesToFind.filter { cls =>
        fields.exists(fld => cls.isAssignableFrom(fld.getType())) ||
        constructors.exists(_.getParameterTypes().exists(c => cls.isAssignableFrom(c))) ||
        constructorAnnotations.exists(a => cls.isAssignableFrom(a.annotationType())) ||
        fieldAnnotations.exists(a => cls.isAssignableFrom(a.annotationType())) ||
        methodAnnotations.exists(a => cls.isAssignableFrom(a.annotationType()))
      }

      val methodParamOrReturnTypeMatches = classesToFind.filter { cls =>
        methods.exists { method =>
          method.getParameterTypes().exists(c => cls.isAssignableFrom(c)) ||
          cls.isAssignableFrom(method.getReturnType())
        }
      }

      if (isAMatches.nonEmpty || hasAMatches.nonEmpty || methodParamOrReturnTypeMatches.nonEmpty) {
        Some(
          Json.obj(
            "inspectedClass" -> Json.fromString(classToInspect.getTypeName),
            "is" -> Json.arr(isAMatches.map(c => Json.fromString(c.toString)): _*),
            "has" -> Json.arr(hasAMatches.map(c => Json.fromString(c.toString)): _*),
            "hasMethodsTakingOrReturning" -> Json.arr(methodParamOrReturnTypeMatches.map(c => Json.fromString(c.toString)): _*)
          )
        )
      } else {
        None
      }
    }.recover { case t: Throwable =>
      Some(
        Json.obj(
          "exceptionThrown" -> Json.fromString(t.toString)
        )
      )
    }.toOption
      .get
  }
}

object FindUsagesOfTypes extends ClassFunctionFactory[Json, FindUsagesOfTypes] {

  final case class Config(classesToFind: Seq[String])

  private implicit val configDecoder: Decoder[Config] = deriveDecoder[Config]

  def create(configJson: Json): FindUsagesOfTypes = {
    val config: Config = configJson.as[Config].toOption.get
    val classesToFind: Seq[Class[_]] = config.classesToFind.flatMap(c => Try(Class.forName(c)).toOption)
    new FindUsagesOfTypes(classesToFind)
  }

}
