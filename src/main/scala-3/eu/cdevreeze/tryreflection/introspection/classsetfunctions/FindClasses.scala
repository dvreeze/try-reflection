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

import eu.cdevreeze.tryreflection.introspection.classsetfunctions.FindClasses.{ClassFilter, ClassSection, Config}
import eu.cdevreeze.tryreflection.introspection.{ClassSetFunctionFactory, ClassSetFunctionReturningJson}
import io.circe.generic.semiauto.deriveDecoder
import io.circe.{Decoder, Json}

import java.lang.reflect.Modifier
import scala.collection.immutable.ListSet
import scala.util.Try

/**
 * Class Set function searching for certain classes, for example, servlet types, Kafka event processor types etc.
 *
 * @author
 *   Chris de Vreeze
 */
final class FindClasses(val config: Config) extends ClassSetFunctionReturningJson:

  def apply(clazzes: Set[Class[?]]): Json =
    val filteredClasses: Set[Class[?]] = clazzes.toSeq
      .filterNot(cls => cls.isInterface && config.ignoreInterface)
      .filterNot(cls => isAbstractClass(cls) && config.ignoreAbstractClass)
      .to(ListSet)

    val classSectionJsons: Seq[Json] = config.classSections
      .map { classSection =>
        val matchingClasses: Set[Class[?]] = filteredClasses.filter(cls => matchFound(cls, classSection))

        Json.obj(
          "groupName" -> Json.fromString(classSection.description),
          "typesFound" -> Json.fromValues(
            matchingClasses.map { cls =>
              Json.obj(
                "name" -> Json.fromString(cls.getName),
                "genericType" -> Json.fromString(cls.toGenericString)
              )
            }
          )
        )
      }

    Json.fromValues(classSectionJsons)
  end apply

  private def matchFound(clazz: Class[?], classSection: ClassSection): Boolean =
    classSection.classFilters.exists {
      case f @ ClassFilter.AssignableTo(_) =>
        isAssignableTo(clazz, f)
      case f @ ClassFilter.UsesType(_) =>
        usesType(clazz, f)
      case f @ ClassFilter.HasAnnotation(_) =>
        hasAnnotation(clazz, f)
    }

  private def isAssignableTo(classToInspect: Class[?], objectFilter: ClassFilter.AssignableTo): Boolean =
    Try(Class.forName(objectFilter.className).isAssignableFrom(classToInspect)).getOrElse(false)

  private def usesType(classToInspect: Class[?], objectFilter: ClassFilter.UsesType): Boolean =
    Try {
      val constructors = classToInspect.getDeclaredConstructors().toSeq
      val fields = classToInspect.getDeclaredFields().toSeq
      val methods = classToInspect.getDeclaredMethods().toSeq

      val typeToFind: Class[?] = Class.forName(objectFilter.className)

      fields.exists(fld => typeToFind.isAssignableFrom(fld.getType())) ||
      constructors.exists(_.getParameterTypes().exists(c => typeToFind.isAssignableFrom(c))) ||
      methods.exists(_.getParameterTypes().exists(c => typeToFind.isAssignableFrom(c))) ||
      methods.exists(m => typeToFind.isAssignableFrom(m.getReturnType))
    }.getOrElse(false)

  private def hasAnnotation(classToInspect: Class[?], objectFilter: ClassFilter.HasAnnotation): Boolean =
    Try {
      val constructors = classToInspect.getDeclaredConstructors().toSeq
      val fields = classToInspect.getDeclaredFields().toSeq
      val methods = classToInspect.getDeclaredMethods().toSeq

      val classAnnotations = classToInspect.getDeclaredAnnotations().toSeq
      val constructorAnnotations = constructors.flatMap(_.getDeclaredAnnotations().toSeq)
      val fieldAnnotations = fields.flatMap(_.getDeclaredAnnotations().toSeq)
      val methodAnnotations = methods.flatMap(_.getDeclaredAnnotations().toSeq)

      val typeToFind: Class[?] = Class.forName(objectFilter.className)

      classAnnotations.exists(a => typeToFind.isAssignableFrom(a.annotationType())) ||
      constructorAnnotations.exists(a => typeToFind.isAssignableFrom(a.annotationType())) ||
      fieldAnnotations.exists(a => typeToFind.isAssignableFrom(a.annotationType())) ||
      methodAnnotations.exists(a => typeToFind.isAssignableFrom(a.annotationType()))
    }.getOrElse(false)

  private def isAbstractClass(clazz: Class[?]): Boolean =
    val isAbstract = Modifier.isAbstract(clazz.getModifiers)
    isAbstract && !clazz.isInterface

object FindClasses extends ClassSetFunctionFactory[Json, FindClasses]:

  enum KindOfFilter:
    case AssignableTo
    case UsesType
    case HasAnnotation

  final case class InternalClassFilter(kindOfFilter: KindOfFilter, filterValue: Json)

  enum ClassFilter(kindOfFilter: KindOfFilter):
    case AssignableTo(className: String) extends ClassFilter(KindOfFilter.AssignableTo)
    case UsesType(className: String) extends ClassFilter(KindOfFilter.UsesType)
    case HasAnnotation(className: String) extends ClassFilter(KindOfFilter.HasAnnotation)

  final case class ClassSection(description: String, classFilters: Seq[ClassFilter])

  final case class Config(ignoreInterface: Boolean = true, ignoreAbstractClass: Boolean = true, classSections: Seq[ClassSection])

  private given Decoder[KindOfFilter] = Decoder.decodeString.emapTry { s =>
    Try {
      s match
        case "AssignableTo"  => KindOfFilter.AssignableTo
        case "UsesType"      => KindOfFilter.UsesType
        case "HasAnnotation" => KindOfFilter.HasAnnotation
        case s               => sys.error(s"Not a KindOfFilter: '$s'")
    }
  }

  private given Decoder[InternalClassFilter] = deriveDecoder[InternalClassFilter]

  private given Decoder[ClassFilter] =
    summon[Decoder[InternalClassFilter]].emapTry { internalClassFilter =>
      internalClassFilter.kindOfFilter match
        case KindOfFilter.AssignableTo =>
          internalClassFilter.filterValue.as[String].toTry.map(s => ClassFilter.AssignableTo(s))
        case KindOfFilter.UsesType =>
          internalClassFilter.filterValue.as[String].toTry.map(s => ClassFilter.UsesType(s))
        case KindOfFilter.HasAnnotation =>
          internalClassFilter.filterValue.as[String].toTry.map(s => ClassFilter.HasAnnotation(s))
    }

  private given Decoder[ClassSection] = deriveDecoder[ClassSection]

  private given Decoder[Config] = deriveDecoder[Config]

  def create(configJson: Json): FindClasses =
    val config: Config = configJson.as[Config].toOption.get
    new FindClasses(config)

end FindClasses
