/*
 * Copyright 2020 Swiss Data Science Center (SDSC)
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

package ch.datascience.tokenrepository.repository.init

import cats.effect._
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.generators.Generators.exceptions
import ch.datascience.interpreters.TestLogger
import ch.datascience.interpreters.TestLogger.Level.{Error, Info}
import ch.datascience.tokenrepository.repository.InMemoryProjectsTokensDb
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class DbInitializerSpec extends WordSpec with InMemoryProjectsTokensDb with MockFactory {

  "run" should {

    "do nothing if projects_tokens table already exists" in new TestCase {
      if (!tableExists()) createTable()

      tableExists() shouldBe true

      (projectPathAdder.run _)
        .expects()
        .returning(IO.unit)

      dbInitializer.run.unsafeRunSync() shouldBe ((): Unit)

      tableExists() shouldBe true

      logger.loggedOnly(Info("Projects Tokens database initialization success"))
    }

    "create the projects_tokens table if id does not exist" in new TestCase {
      if (tableExists()) dropTable()

      tableExists() shouldBe false

      (projectPathAdder.run _)
        .expects()
        .returning(IO.unit)

      dbInitializer.run.unsafeRunSync() shouldBe ((): Unit)

      tableExists() shouldBe true

      logger.loggedOnly(Info("Projects Tokens database initialization success"))
    }

    "fail if the Projects Paths adding process fails" in new TestCase {
      if (tableExists()) dropTable()

      tableExists() shouldBe false

      val exception = exceptions.generateOne
      (projectPathAdder.run _)
        .expects()
        .returning(IO raiseError exception)

      intercept[Exception] {
        dbInitializer.run.unsafeRunSync() shouldBe ((): Unit)
      } shouldBe exception

      logger.loggedOnly(Error("Projects Tokens database initialization failure", exception))
    }
  }

  private trait TestCase {
    val logger           = TestLogger[IO]()
    val projectPathAdder = mock[IOProjectPathAdder]
    val dbInitializer    = new DbInitializer[IO](projectPathAdder, transactor, logger)
  }
}
