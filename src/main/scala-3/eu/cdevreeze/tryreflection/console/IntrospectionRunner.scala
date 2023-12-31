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

package eu.cdevreeze.tryreflection.console

import eu.cdevreeze.tryreflection.internal.LineGroup
import eu.cdevreeze.tryreflection.support.Introspector

/**
 * Introspector, generating javap-like output for a given class or interface.
 *
 * @author
 *   Chris de Vreeze
 */
object IntrospectionRunner:

  private val exampleClasses: Seq[Class[?]] = Seq(
    classOf[AnyRef],
    classOf[String],
    classOf[java.lang.Enum[?]],
    classOf[java.util.List[?]],
    classOf[java.util.ArrayList[?]],
    classOf[java.util.Map[?, ?]],
    classOf[java.util.Map.Entry[?, ?]],
    classOf[java.util.HashMap[?, ?]],
    classOf[java.util.TreeMap[?, ?]],
    classOf[scala.Product],
    classOf[scala.collection.immutable.Seq[?]],
    classOf[scala.collection.immutable.List[?]],
    classOf[scala.util.CommandLineParser.type],
    classOf[scala.jdk.javaapi.CollectionConverters.type],
    classOf[scala.io.Source],
    classOf[scala.io.Source.type]
  )

  def main(args: Array[String]): Unit =
    if args.nonEmpty then introspect(args.head)
    else
      exampleClasses.foreach { cls =>
        println()
        println(introspect(cls).makeString)
      }

  def introspect(className: String): Unit =
    val clazz: Class[?] = Class.forName(className)
    println(introspect(clazz).makeString)

  def introspect(clazz: Class[?]): LineGroup =
    require(isNormalClass(clazz) || clazz.isInterface)
    if isNormalClass(clazz) then introspectNormalClass(clazz) else introspectInterface(clazz)

  private def introspectNormalClass(clazz: Class[?]): LineGroup =
    require(isNormalClass(clazz))
    Introspector.forClass(clazz).introspect

  private def introspectInterface(clazz: Class[?]): LineGroup =
    require(clazz.isInterface)
    Introspector.forInterface(clazz).introspect

  private def isNormalClass(clazz: Class[?]): Boolean =
    !clazz.isInterface && !clazz.isEnum && !clazz.isArray && !clazz.isAnnotation && !clazz.isPrimitive

end IntrospectionRunner
