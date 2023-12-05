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

package eu.cdevreeze.tryreflection.introspection

import eu.cdevreeze.tryreflection.introspection.Rule.RuleResult

import java.lang.reflect.{Field, Method, Type}

/**
 * Introspection rule.
 *
 * @author
 *   Chris de Vreeze
 */
trait Rule:

  def introspect(classes: Seq[Class[_]]): RuleResult

object Rule:

  enum Status:
    case Ok, Warn, Error

  enum Result(status: Status, message: String, context: Seq[Class[_]]):
    case MethodResult(m: Method, status: Status, message: String, context: Seq[Class[_]]) extends Result(status, message, context)
    case FieldResult(f: Field, status: Status, message: String, context: Seq[Class[_]]) extends Result(status, message, context)
    case ClassResult(c: Class[_], status: Status, message: String, context: Seq[Class[_]]) extends Result(status, message, context)
    case TypeResult(tpe: Type, status: Status, message: String, context: Seq[Class[_]]) extends Result(status, message, context)
    case OtherResult(status: Status, message: String, context: Seq[Class[_]]) extends Result(status, message, context)

  final case class RuleResult(results: Seq[Result])

end Rule
