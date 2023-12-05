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

import eu.cdevreeze.tryreflection.introspection.Rule.RuleResult
import eu.cdevreeze.tryreflection.introspection.rules.ShowSupertypes

/**
 * Program running rule ShowSupertypes.
 *
 * @author
 *   Chris de Vreeze
 */
object ShowSupertypesRunner:

  def main(args: Array[String]): Unit =
    val classes: Seq[Class[_]] = args.toSeq.map(Class.forName)
    val ruleResult: RuleResult = ShowSupertypes.introspect(classes)
    ruleResult.results.foreach(println)
