/*
 * Copyright 2019 Swiss Data Science Center (SDSC)
 * A partnership between École Polytechnique Fédérale de Lausanne (EPFL) and
 * Eidgenössische Technische Hochschule Zürich (ETHZ).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

organization := "io.renku"
name := "json-ld"
scalaVersion := "2.12.8"

val circeVersion = "0.11.1"
libraryDependencies += "io.circe" %% "circe-core"    % circeVersion
libraryDependencies += "io.circe" %% "circe-literal" % circeVersion
libraryDependencies += "io.circe" %% "circe-parser"  % circeVersion

libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.1"

// Test dependencies
libraryDependencies += "eu.timepit"     %% "refined"    % "0.9.9"  % Test
libraryDependencies += "org.scalamock"  %% "scalamock"  % "4.4.0"  % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
libraryDependencies += "org.scalatest"  %% "scalatest"  % "3.0.8"  % Test
