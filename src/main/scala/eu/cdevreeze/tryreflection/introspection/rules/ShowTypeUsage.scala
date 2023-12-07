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

import eu.cdevreeze.tryreflection.introspection.ClassUniverseRule
import io.circe.Json

/**
 * Rule that iterates over the "class universe", searching for usage of the given types, in class/interface names, names of superclass or
 * implemented interfaces, method signatures, constructor signatures, field types, etc.
 *
 * @author
 *   Chris de Vreeze
 */
final class ShowTypeUsage(val classes: Seq[Class[_]], classUniverse: Seq[Class[_]]) extends ClassUniverseRule(classUniverse):

  def run(): Json =
    val results: Seq[Json] =
      for {
        classToInspect <- classUniverse
        classToFind <- classes
      } yield run(classToFind, classToInspect)
    Json.fromValues(results)

  def run(classToFind: Class[_], classToInspect: Class[_]): Json =
    val superclassOption = Option(classToInspect.getSuperclass())
    val interfaces = classToInspect.getInterfaces().toSeq
    val constructors = classToInspect.getDeclaredConstructors()
    val fields = classToInspect.getDeclaredFields().toSeq
    val methods = classToInspect.getDeclaredMethods().toSeq

    val classMatches = classesMatch(classToFind, classToInspect)
    val superclassMatches = superclassOption.exists(cls => classesMatch(classToFind, cls))
    val matchingInterfaces = interfaces.filter(itf => classesMatch(classToFind, itf))
    val matchingConstructors = constructors.filter(_.getParameterTypes().exists(c => classesMatch(classToFind, c)))
    val matchingFields = fields.filter(fld => classesMatch(classToFind, fld.getType()))
    val matchingMethods = methods.filter { method =>
      method.getParameterTypes().exists(c => classesMatch(classToFind, c)) ||
      classesMatch(classToFind, method.getReturnType())
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
  end run

  private def classesMatch(class1: Class[_], class2: Class[_]): Boolean =
    class1.isAssignableFrom(class2) && class2.isAssignableFrom(class1)

end ShowTypeUsage
