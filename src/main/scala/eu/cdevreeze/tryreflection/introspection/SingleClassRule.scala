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

import io.circe.Json

import scala.util.chaining.scalaUtilChainingOps

/**
 * Introspection rule taking a single Class as input.
 *
 * @author
 *   Chris de Vreeze
 */
trait SingleClassRule extends Rule:

  final def introspect(classes: Seq[Class[_]]): Json =
    classes.map(clazz => introspect(clazz)).pipe(Json.arr)

  def introspect(clazz: Class[_]): Json
