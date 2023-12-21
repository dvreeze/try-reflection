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

package eu.cdevreeze.tryreflection.internal

import java.lang.reflect.{ParameterizedType, Type}

/**
 * Java reflection utility methods that are commonly used.
 *
 * @author
 *   Chris de Vreeze
 */
object ReflectionUtils {

  /**
   * Finds the types of all super-classes of this class. If the parameter Class represents an interface, an empty collection is returned.
   * Only Type instances for classes are returned, not for interfaces.
   *
   * Even if an interface extends a class according to the source code, this will not be found by reflection when calling this method on a
   * Class instance representing that interface.
   */
  def findSuperclasses(clazz: Class[_]): Seq[Type] = {
    // Returns None if this Class instance represents an interface
    val superclassOption: Option[Type] = Option(clazz.getGenericSuperclass())
    // Recursion
    superclassOption
      .map(cls => toClassOption(cls).toSeq.flatMap(findSuperclasses).prepended(cls))
      .getOrElse(Seq.empty)
      .distinct
  }

  /**
   * Finds the types of all interfaces implemented/extended directly or indirectly by the class or interface represented by the parameter
   * Class. If the parameter Class represents a class rather than an interface, only interfaces found directly or indirectly from this class
   * are returned, and not those found from super-classes of this class.
   */
  def findInterfacesIgnoringSuperclasses(clazz: Class[_]): Seq[Type] = {
    val interfaces: Seq[Type] = clazz.getGenericInterfaces().toSeq
    // Recursion
    interfaces
      .appendedAll(
        interfaces.flatMap(toClassOption).flatMap(findInterfacesIgnoringSuperclasses)
      )
      .distinct
  }

  /**
   * Finds all interfaces implemented/extended directly or indirectly by the class or interface represented by the parameter Class, or by
   * one of its super-classes. Hence this method returns Type instances for all interfaces implemented/extended directly or indirectly,
   * taking super-classes into account as well.
   */
  def findAllInterfaces(clazz: Class[_]): Seq[Type] = {
    findSuperclasses(clazz)
      .prepended(clazz)
      .flatMap(toClassOption)
      .flatMap(findInterfacesIgnoringSuperclasses)
      .distinct
  }

  private def toClassOption(tpe: Type): Option[Class[_]] = {
    tpe match {
      case cls: Class[_]            => Option(cls)
      case ptype: ParameterizedType => Option(ptype.getRawType).collect { case cls: Class[_] => cls }
      case _                        => None
    }
  }
}
