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

package ch.datascience.triplesgenerator.eventprocessing.filelog

import java.nio.file.{Files, OpenOption, StandardOpenOption}
import java.util.concurrent.ConcurrentHashMap

import cats.effect._
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.generators.Generators._
import ch.datascience.interpreters.TestLogger
import ch.datascience.interpreters.TestLogger.Level.Info
import ch.datascience.triplesgenerator.eventprocessing.EventsSource
import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.WordSpec
import org.scalatest.concurrent.{Eventually, IntegrationPatience}

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps

class FileEventProcessorRunnerSpec extends WordSpec with Eventually with IntegrationPatience with MockFactory {

  private implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  "file event source" should {

    "send every new line in a file to the registered processor and process them concurrently" in new TestCase {
      val fileLines = nonEmptyStringsList(10).generateOne

      writeToFile(fileLines)

      val accumulator = new ConcurrentHashMap[String, Long]()
      def processor(line: String): IO[Unit] = {
        accumulator.put(line, Thread.currentThread().getId)
        IO.unit
      }

      eventsSource.withEventsProcessor(processor).run.unsafeRunCancelable(_ => Unit)

      eventually {
        accumulator.keySet().asScala shouldBe fileLines.toSet
      }

      val laterAddedLine = nonEmptyStrings().generateOne
      writeToFile(laterAddedLine)

      eventually {
        accumulator.keySet().asScala shouldBe (fileLines :+ laterAddedLine).toSet
      }

      withClue("Number of used threads has to be greater than 1, in fact ") {
        accumulator.values().asScala.toSet.size should be > 1
      }

      logger.loggedOnly(Info("Listening for new events"))
    }

    "continue if there is an error during processing" in new TestCase {
      val line1 = nonEmptyStrings().generateOne
      val line2 = nonEmptyStrings().generateOne
      val line3 = nonEmptyStrings().generateOne

      writeToFile(Seq(line1, line2, line3))

      val accumulator = new ConcurrentHashMap[String, Long]()
      def processor(line: String): IO[Unit] =
        if (line == line2) IO.raiseError(new Exception("error during processing line2"))
        else {
          accumulator.put(line, Thread.currentThread().getId)
          IO.unit
        }

      eventsSource.withEventsProcessor(processor).run.unsafeRunCancelable(_ => Unit)

      eventually {
        accumulator.keySet().asScala shouldBe Set(line1, line3)
      }

      logger.loggedOnly(Info("Listening for new events"))
    }

    "fail if the log file cannot be found in the config" in {
      class IOLogFileConfigProvider extends LogFileConfigProvider[IO]
      val configProvider = mock[IOLogFileConfigProvider]
      val newRunner      = new FileEventProcessorRunner(_, configProvider, TestLogger[IO]())
      val eventsSource   = new EventsSource[IO](newRunner)
      def processor(line: String): IO[Unit] = IO.unit

      val exception = exceptions.generateOne
      (configProvider.get _)
        .expects()
        .returning(IO.raiseError(exception))

      intercept[Exception] {
        eventsSource.withEventsProcessor(processor).run.unsafeRunSync
      } shouldBe exception
    }
  }

  private val openOptions: Seq[OpenOption] = Seq(
    StandardOpenOption.WRITE,
    StandardOpenOption.CREATE,
    StandardOpenOption.APPEND,
    StandardOpenOption.SYNC
  )

  private trait TestCase {
    val eventLogFile = Files.createTempFile("test-events", "log")
    private val config = ConfigFactory.parseMap(
      Map(
        "file-event-log" -> Map(
          "file-path" -> eventLogFile.toFile.getAbsolutePath
        ).asJava
      ).asJava
    )
    private val configProvider = new LogFileConfigProvider[IO](config)
    val logger                 = TestLogger[IO]()
    private val newRunner      = new FileEventProcessorRunner(_, configProvider, logger)
    val eventsSource           = new EventsSource[IO](newRunner)

    def writeToFile(item: String): Unit = writeToFile(Seq(item))

    def writeToFile(items: Seq[String]): Unit =
      Files.write(eventLogFile, items.asJavaCollection, openOptions: _*)
  }
}