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

import eu.cdevreeze.tryreflection.introspection.classsetfunctions.FindClasses.{ClassFilter, Config}
import eu.cdevreeze.tryreflection.introspection.{ClassSetFunctionFactory, ClassSetFunctionReturningJson}
import io.circe.generic.semiauto.deriveDecoder
import io.circe.{Decoder, Json}

import java.lang.reflect.Modifier
import scala.util.Try

/**
 * Class Set function searching for certain classes, for example, servlet types, Kafka event processor types etc.
 *
 * @author
 *   Chris de Vreeze
 */
final class FindClasses(val config: Config) extends ClassSetFunctionReturningJson {

  def apply(clazzes: Set[Class[_]]): Json = {
    val resultsPerClass: Seq[Json] = clazzes.toSeq.flatMap { cls =>
      Try(applyForClass(cls)).toOption.filterNot(_ == Json.obj())
    }
    Json.fromValues(resultsPerClass)
  }

  private def applyForClass(clazz: Class[_]): Json = {
    if ((clazz.isInterface && config.ignoreInterface) || (isAbstractClass(clazz) && config.ignoreAbstractClass)) {
      Json.obj()
    } else {
      val matchingSectionDescriptions: Seq[String] =
        config.classSections
          .filter { classSection =>
            classSection.classFilters.exists {
              case f @ ClassFilter.AssignableTo(_) =>
                isAssignableTo(clazz, f)
              case f @ ClassFilter.HasPublicMethod(_, _, _) =>
                hasPublicMethod(clazz, f)
            }
          }
          .map(_.description)
          .distinct

      if (matchingSectionDescriptions.isEmpty) Json.obj()
      else {
        Json.obj(
          "className" -> Json.fromString(clazz.getTypeName),
          "class" -> Json.fromString(clazz.toGenericString),
          "is" -> Json.fromValues(matchingSectionDescriptions.sorted.map(Json.fromString))
        )
      }
    }
  }

  private def isAssignableTo(classToInspect: Class[_], objectFilter: ClassFilter.AssignableTo): Boolean = {
    Try(Class.forName(objectFilter.className).isAssignableFrom(classToInspect)).getOrElse(false)
  }

  private def hasPublicMethod(classToInspect: Class[_], objectFilter: ClassFilter.HasPublicMethod): Boolean = {
    sys.error(s"Not yet implemented (finding by method, that is)")
  }

  private def isAbstractClass(clazz: Class[_]): Boolean = {
    val isAbstract = Modifier.isAbstract(clazz.getModifiers)
    isAbstract && !clazz.isInterface
  }
}

object FindClasses extends ClassSetFunctionFactory[Json, FindClasses] {

  sealed trait KindOfFilter

  object KindOfFilter {
    case object AssignableTo extends KindOfFilter
    case object HasPublicMethod extends KindOfFilter
  }

  final case class InternalClassFilter(kindOfFilter: KindOfFilter, filterValue: Json)

  final case class InternalMethodSignature(methodName: String, returnType: String, parameterTypes: Seq[String])

  sealed trait ClassFilter {
    def kindOfFilter: KindOfFilter
  }

  object ClassFilter {
    final case class AssignableTo(className: String) extends ClassFilter {
      def kindOfFilter: KindOfFilter = KindOfFilter.AssignableTo
    }
    final case class HasPublicMethod(methodName: String, returnType: String, parameterTypes: Seq[String]) extends ClassFilter {
      def kindOfFilter: KindOfFilter = KindOfFilter.HasPublicMethod
    }
  }

  final case class ClassSection(description: String, classFilters: Seq[ClassFilter])

  final case class Config(ignoreInterface: Boolean = true, ignoreAbstractClass: Boolean = true, classSections: Seq[ClassSection])

  private implicit val kindOfFilterDecoder: Decoder[KindOfFilter] = Decoder.decodeString.emapTry { s =>
    Try {
      s match {
        case "AssignableTo"    => KindOfFilter.AssignableTo
        case "HasPublicMethod" => KindOfFilter.HasPublicMethod
        case s                 => sys.error(s"Not a KindOfFilter: '$s'")
      }
    }
  }

  private implicit val internalClassFilterDecoder: Decoder[InternalClassFilter] = deriveDecoder[InternalClassFilter]

  private implicit val internalMethodSignatureDecoder: Decoder[InternalMethodSignature] =
    deriveDecoder[InternalMethodSignature]

  private implicit val classFilterDecoder: Decoder[ClassFilter] =
    internalClassFilterDecoder.emapTry { internalClassFilter =>
      internalClassFilter.kindOfFilter match {
        case KindOfFilter.AssignableTo =>
          internalClassFilter.filterValue.as[String].toTry.map(s => ClassFilter.AssignableTo(s))
        case KindOfFilter.HasPublicMethod =>
          internalClassFilter.filterValue
            .as[InternalMethodSignature]
            .toTry
            .map(m => ClassFilter.HasPublicMethod(m.methodName, m.returnType, m.parameterTypes))
      }
    }

  private implicit val classSectionDecoder: Decoder[ClassSection] = deriveDecoder[ClassSection]

  private implicit val configDecoder: Decoder[Config] = deriveDecoder[Config]

  def create(configJson: Json): FindClasses = {
    val config: Config = configJson.as[Config].toOption.get
    new FindClasses(config)
  }
}
