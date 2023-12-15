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

/**
 * Support for generating lines with indentation.
 *
 * @author
 *   Chris de Vreeze
 */
final case class LineGroup(indent: Int, lines: Seq[Line]):

  def makeString: String = effectiveLines.map(_.makeString).mkString("\n")

  /**
   * Adds numberOfSpaces to the indentation. This is a cheap operation involving no String concatenation.
   */
  def shiftRight(numberOfSpaces: Int): LineGroup = withIndent(indent + numberOfSpaces)

  def appendStringToLastLine(s: String): LineGroup =
    if lines.isEmpty then this
    else LineGroup(indent, lines.updated(lines.size - 1, lines.last.appendString(s)))

  private def withIndent(newIndent: Int): LineGroup = LineGroup(newIndent, lines)

  private def effectiveLines: Seq[Line] = lines.map(_.shiftRight(indent))

object LineGroup:

  private val space: String = ' '.toString

  val fourSpaces: String = space * 4
  val javaIndent = 4

  def apply(lines: Seq[Line]): LineGroup = LineGroup(0, lines)

  def of(lines: String*): LineGroup = apply(lines.map(Line.apply))

  /**
   * Creates a LineGroup from several LineGroup instances. This is a cheap operation involving no String concatenation.
   */
  def from(lineGroups: LineGroup*): LineGroup = LineGroup(lineGroups.flatMap(_.effectiveLines))

end LineGroup
