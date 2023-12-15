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
 * Support for generating a single line with indentation.
 *
 * @author
 *   Chris de Vreeze
 */
final case class Line(indent: Int, line: String) {

  def makeString: String = s"${Line.space * indent}$line"

  /**
   * Adds numberOfSpaces to the indentation. This is a cheap operation involving no String concatenation.
   */
  def shiftRight(numberOfSpaces: Int): Line = withIndent(indent + numberOfSpaces)

  def appendString(s: String): Line = Line(indent, line + s)

  private def withIndent(newIndent: Int): Line = Line(newIndent, line)
}

object Line {

  private val space: String = ' '.toString

  def apply(line: String): Line = Line(0, line)

}
