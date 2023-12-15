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

package eu.cdevreeze.tryreflection.support

import eu.cdevreeze.tryreflection.internal.LineGroup

import java.lang.reflect.{Constructor, Field, Method, Modifier, Type, TypeVariable}
import scala.util.chaining.scalaUtilChainingOps

/**
 * Introspector, generating javap-like output for a given class, interface, etc.
 *
 * @author
 *   Chris de Vreeze
 */
object Introspector:

  sealed trait Introspector:
    def introspect: LineGroup

  final class ClassIntrospector(val clazz: Class[?]) extends Introspector:
    require(isNormalClass(clazz))

    def introspect: LineGroup =
      LineGroup.from(
        getClassNameWithSuperTypes.appendStringToLastLine(" {"),
        LineGroup
          .from(
            LineGroup.from(clazz.getDeclaredClasses().toSeq.filter(_.isInterface).map(c => forInterface(c).introspect): _*),
            LineGroup.from(clazz.getDeclaredClasses().toSeq.filterNot(_.isInterface).map(c => forClass(c).introspect): _*),
            LineGroup.from(clazz.getDeclaredConstructors().toSeq.map(c => forConstructor(c).introspect): _*),
            LineGroup.from(clazz.getDeclaredFields().toSeq.map(f => forField(f).introspect): _*),
            LineGroup.from(clazz.getDeclaredMethods().toSeq.map(m => forMethod(m).introspect): _*)
          )
          .shiftRight(LineGroup.javaIndent),
        LineGroup.of("}")
      )

    private def getClassNameWithSuperTypes: LineGroup =
      val nameWithModifiers = getClassNameWithModifiers
      val superclassOption: Option[Type] = Option(clazz.getGenericSuperclass())
      val extendedInterfaces = clazz.getGenericInterfaces().toSeq
      val firstLine: String = (superclassOption, extendedInterfaces) match
        case (None, Nil) => nameWithModifiers
        case (None, _) =>
          s"$nameWithModifiers implements ${extendedInterfaces.map(_.getTypeName).mkString(", ")}"
        case (Some(tpe), Nil) =>
          s"$nameWithModifiers extends ${tpe.getTypeName}"
        case (Some(tpe), _) =>
          s"$nameWithModifiers extends ${tpe.getTypeName} implements ${extendedInterfaces.map(_.getTypeName).mkString(", ")}"
      LineGroup.of(firstLine)

    private def getClassNameWithModifiers: String =
      val modifiersString = getModifiersAsString
      val fullClassName = clazz.getTypeName()
      val typeParams = clazz.getTypeParameters.toSeq
      s"$modifiersString class $fullClassName"
        .pipe(s => if typeParams.isEmpty then s else s"$s<${typeParams.map(tp => show(tp)).mkString(", ")}>")

    private def getModifiersAsString: String =
      val mods = clazz.getModifiers()
      val isPublic = Modifier.isPublic(mods)
      val isProtected = Modifier.isProtected(mods)
      val isPrivate = Modifier.isPrivate(mods)
      val isStatic = Modifier.isStatic(mods)
      val isFinal = Modifier.isFinal(mods)
      val isAbstract = Modifier.isAbstract(mods)

      val accessModifierString: String =
        if isPublic then "public"
        else if isProtected then "protected"
        else if isPrivate then "private"
        else ""

      val staticModifierString: String = if isStatic then "static" else ""

      val finalOrAbstractModifierString: String =
        if isFinal then "final"
        else if isAbstract then "abstract"
        else ""

      Seq(accessModifierString, staticModifierString, finalOrAbstractModifierString)
        .filter(_.nonEmpty)
        .mkString(" ")
    end getModifiersAsString

  end ClassIntrospector

  final class InterfaceIntrospector(val clazz: Class[?]) extends Introspector:
    require(clazz.isInterface)

    def introspect: LineGroup =
      LineGroup.from(
        getInterfaceNameWithSuperTypes.appendStringToLastLine(" {"),
        LineGroup
          .from(
            LineGroup.from(clazz.getDeclaredClasses().toSeq.filter(_.isInterface).map(c => forInterface(c).introspect): _*),
            LineGroup.from(clazz.getDeclaredClasses().toSeq.filterNot(_.isInterface).map(c => forClass(c).introspect): _*),
            LineGroup.from(clazz.getDeclaredFields().toSeq.map(f => forField(f).introspect): _*),
            LineGroup.from(clazz.getDeclaredMethods().toSeq.map(m => forMethod(m).introspect): _*)
          )
          .shiftRight(LineGroup.javaIndent),
        LineGroup.of("}")
      )

    private def getInterfaceNameWithSuperTypes: LineGroup =
      val nameWithModifiers = getInterfaceNameWithModifiers
      val extendedInterfaces = clazz.getGenericInterfaces().toSeq
      val firstLine: String = extendedInterfaces match
        case Nil => nameWithModifiers
        case _   => s"$nameWithModifiers extends ${extendedInterfaces.map(_.getTypeName).mkString(", ")}"
      LineGroup.of(firstLine)

    private def getInterfaceNameWithModifiers: String =
      val modifiersString = getModifiersAsString
      val fullClassName = clazz.getTypeName()
      val typeParams = clazz.getTypeParameters.toSeq
      s"$modifiersString interface $fullClassName"
        .pipe(s => if typeParams.isEmpty then s else s"$s<${typeParams.map(tp => show(tp)).mkString(", ")}>")

    private def getModifiersAsString: String =
      val mods = clazz.getModifiers()
      val isPublic = Modifier.isPublic(mods)
      val isProtected = Modifier.isProtected(mods)
      val isPrivate = Modifier.isPrivate(mods)
      val isStatic = Modifier.isStatic(mods)

      val accessModifierString: String =
        if isPublic then "public"
        else if isProtected then "protected"
        else if isPrivate then "private"
        else ""

      val staticModifierString: String = if isStatic then "static" else ""

      Seq(accessModifierString, staticModifierString)
        .filter(_.nonEmpty)
        .mkString(" ")
    end getModifiersAsString

  end InterfaceIntrospector

  final class MethodIntrospector(val method: Method) extends Introspector:

    def introspect: LineGroup = LineGroup.of(method.toGenericString + ";")

  final class ConstructorIntrospector(val constructor: Constructor[?]) extends Introspector:

    def introspect: LineGroup = LineGroup.of(constructor.toGenericString + ";")

  final class FieldIntrospector(val field: Field) extends Introspector:

    def introspect: LineGroup = LineGroup.of(field.toGenericString + ";")

  private def show(typeVar: TypeVariable[?]): String =
    val name = typeVar.getName
    val bounds: Array[Type] = typeVar.getBounds
    if bounds.isEmpty then name else s"$name extends ${bounds.mkString(" & ")}"

  /**
   * Returns true if the parameter Class does not refer to an interface, array type, annotation or primitive.
   */
  def isNormalClass(clazz: Class[?]): Boolean =
    !clazz.isInterface && !clazz.isArray && !clazz.isAnnotation && !clazz.isPrimitive

  def forClass(clazz: Class[?]): ClassIntrospector = ClassIntrospector(clazz)

  def forInterface(clazz: Class[?]): InterfaceIntrospector = InterfaceIntrospector(clazz)

  def forMethod(method: Method): MethodIntrospector = MethodIntrospector(method)

  def forConstructor(cons: Constructor[?]): ConstructorIntrospector = ConstructorIntrospector(cons)

  def forField(fld: Field): FieldIntrospector = FieldIntrospector(fld)

end Introspector
