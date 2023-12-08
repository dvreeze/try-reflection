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

import eu.cdevreeze.tryreflection.introspection.classfunctions.FindUsagesOfType
import io.circe.Json
import org.burningwave.core.assembler.ComponentContainer
import org.burningwave.core.classes.SearchConfig
import org.burningwave.core.io.FileSystemItem

import scala.jdk.CollectionConverters.*
import scala.util.Using

/**
 * Program running rule ShowTypeUsage.
 *
 * TODO Rename
 *
 * @author
 *   Chris de Vreeze
 */
object ShowTypeUsageRunner:

  def main(args: Array[String]): Unit =
    require(args.nonEmpty, "Usage: ShowTypeUsageRunner --classToFind <class to find> ... --packageNameFilter <pkg name filter> ...")
    require(args.length % 2 == 0, "Expected even number of arguments")
    val argPairs = args.toSeq.grouped(2).toSeq

    val classesToFind: Seq[Class[_]] =
      argPairs.filter(_.head == "--classToFind").map(_(1)).map(Class.forName)
    val packageNameFilters: Set[String] =
      argPairs.filter(_.head == "--packageNameFilter").map(_(1)).toSet

    val classUniverse: Seq[Class[_]] = findClassesToInspect(packageNameFilters)

    val classFunction = FindUsagesOfType(classesToFind)
    val jsons: Seq[Json] = classUniverse.map(cls => classFunction(cls))
    val result: Json = Json.fromValues(jsons)
    println(result)
  end main

  private def findClassesToInspect(packageNameFilters: Set[String]): Seq[Class[_]] =
    val componentSupplier = ComponentContainer.getInstance()
    val pathHelper = componentSupplier.getPathHelper()
    val classHunter = componentSupplier.getClassHunter()

    val searchConfig = SearchConfig
      .forPaths(
        pathHelper.getAllMainClassPaths
      )
      .addFileFilter(
        FileSystemItem.Criteria.forAllFileThat { fileSystemItem =>
          Option(fileSystemItem.toJavaClass)
            .flatMap(cls => Option(cls.getPackageName))
            .exists(pkgName => packageNameFilters.exists(f => pkgName.contains(f)))
        }
      )

    Using.resource(classHunter.findBy(searchConfig))(_.getClasses.asScala.toList)
  end findClassesToInspect

end ShowTypeUsageRunner
