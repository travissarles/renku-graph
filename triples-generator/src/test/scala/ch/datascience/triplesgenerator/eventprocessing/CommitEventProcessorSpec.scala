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

package ch.datascience.triplesgenerator.eventprocessing

import EventProcessingGenerators._
import cats.MonadError
import cats.data.EitherT.{leftT, rightT}
import cats.data.{EitherT, NonEmptyList}
import cats.effect.{ContextShift, IO, Timer}
import cats.implicits._
import ch.datascience.control.Throttler
import ch.datascience.generators.CommonGraphGenerators._
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.generators.Generators._
import ch.datascience.graph.model.EventsGenerators._
import ch.datascience.graph.model.events._
import ch.datascience.graph.model.projects.Id
import ch.datascience.graph.tokenrepository.{AccessTokenFinder, IOAccessTokenFinder}
import ch.datascience.http.client.AccessToken
import ch.datascience.interpreters.TestLogger.Level.{Error, Info}
import ch.datascience.interpreters.TestLogger.Matcher.NotRefEqual
import ch.datascience.interpreters.{TestDbTransactor, TestLogger}
import ch.datascience.logging.TestExecutionTimeRecorder
import ch.datascience.metrics.MetricsRegistry
import ch.datascience.rdfstore.{JsonLDTriples, SparqlQueryTimeRecorder}
import ch.datascience.triplesgenerator.eventprocessing.CommitEvent.{CommitEventWithParent, CommitEventWithoutParent}
import ch.datascience.triplesgenerator.eventprocessing.CommitEventProcessor.ProcessingRecoverableError
import ch.datascience.triplesgenerator.eventprocessing.IOCommitEventProcessor.eventsProcessingTimesBuilder
import ch.datascience.triplesgenerator.eventprocessing.triplescuration.CuratedTriples
import ch.datascience.triplesgenerator.eventprocessing.triplescuration.CurationGenerators._
import ch.datascience.triplesgenerator.eventprocessing.triplescuration.IOTriplesCurator.CurationRecoverableError
import ch.datascience.triplesgenerator.eventprocessing.triplescuration.interpreters.TryTriplesCurator
import ch.datascience.triplesgenerator.eventprocessing.triplesgeneration.TriplesGenerator
import ch.datascience.triplesgenerator.eventprocessing.triplesgeneration.TriplesGenerator.GenerationRecoverableError
import ch.datascience.triplesgenerator.eventprocessing.triplesuploading.TriplesUploadResult._
import ch.datascience.triplesgenerator.eventprocessing.triplesuploading.TryUploader
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Positive
import io.prometheus.client.Histogram
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.concurrent.{Eventually, IntegrationPatience}
import org.scalatest.{Assertion, WordSpec}

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds
import scala.util.Try

class CommitEventProcessorSpec extends WordSpec with MockFactory with Eventually with IntegrationPatience {
  import IOAccessTokenFinder._

  "process" should {

    "succeed events are successfully turned into triples and all stored in Jena" in new TestCase {

      val commitEvents = commitsLists().generateOne

      givenFetchingAccessToken(forProjectId = commitEvents.head.project.id)
        .returning(context.pure(maybeAccessToken))

      val commitsAndTriples = generateTriples(forCommits = commitEvents)

      commitsAndTriples.toList foreach successfulTriplesGenerationAndUpload

      expectEventMarkedDone(commitEvents.head.compoundEventId)

      eventProcessor.process(eventId, commitEvents) shouldBe context.unit

      logSummary(commitEvents, uploaded = commitsAndTriples.size, failed = 0)

      verifyMetricsCollected()
    }

    "succeed if events are successfully turned into triples and all stored in Jena " +
      "even if some of them failed in different stages" in new TestCase {

      val commitEvents                         = commitsLists(size = Gen.const(3)).generateOne
      val commit1 +: commit2 +: commit3 +: Nil = commitEvents.toList

      givenFetchingAccessToken(forProjectId = commitEvents.head.project.id)
        .returning(context.pure(maybeAccessToken))

      val successfulCommitsAndTriples = generateTriples(forCommits = NonEmptyList.of(commit1, commit3))

      successfulCommitsAndTriples.toList foreach successfulTriplesGenerationAndUpload

      val exception2 = exceptions.generateOne
      (triplesFinder
        .generateTriples(_: CommitEvent)(_: Option[AccessToken]))
        .expects(commit2, maybeAccessToken)
        .returning(EitherT.liftF[Try, ProcessingRecoverableError, JsonLDTriples](context.raiseError(exception2)))

      expectEventMarkedAsNonRecoverableFailure(commit2.compoundEventId, exception2)

      eventProcessor.process(eventId, commitEvents) shouldBe context.unit

      logError(commit2, exception2)
      logSummary(commitEvents, uploaded = successfulCommitsAndTriples.size, failed = 1)
    }

    "succeed and mark event with NonRecoverableFailure if finding triples fails" in new TestCase {

      val commitEvents  = commitsLists(size = Gen.const(1)).generateOne
      val commit +: Nil = commitEvents.toList

      givenFetchingAccessToken(forProjectId = commitEvents.head.project.id)
        .returning(context.pure(maybeAccessToken))

      val exception = exceptions.generateOne
      (triplesFinder
        .generateTriples(_: CommitEvent)(_: Option[AccessToken]))
        .expects(commit, maybeAccessToken)
        .returning(EitherT.liftF[Try, ProcessingRecoverableError, JsonLDTriples](context.raiseError(exception)))

      expectEventMarkedAsNonRecoverableFailure(commit.compoundEventId, exception)

      eventProcessor.process(eventId, commitEvents) shouldBe context.unit

      logError(commitEvents.head, exception)
      logSummary(commitEvents, uploaded = 0, failed = 1)
    }

    s"succeed and mark event with RecoverableFailure if finding triples fails with $GenerationRecoverableError" in new TestCase {

      val commitEvents  = commitsLists(size = Gen.const(1)).generateOne
      val commit +: Nil = commitEvents.toList

      givenFetchingAccessToken(forProjectId = commitEvents.head.project.id)
        .returning(context.pure(maybeAccessToken))

      val exception = GenerationRecoverableError(nonBlankStrings().generateOne.value)
      (triplesFinder
        .generateTriples(_: CommitEvent)(_: Option[AccessToken]))
        .expects(commit, maybeAccessToken)
        .returning(leftT[Try, JsonLDTriples](exception))

      expectEventMarkedAsRecoverableFailure(commit.compoundEventId, exception)

      eventProcessor.process(eventId, commitEvents) shouldBe context.unit

      logError(commitEvents.head, exception.message)
      logSummary(commitEvents, uploaded = 0, failed = 1)
    }

    s"succeed and mark event with RecoverableFailure if curating triples fails with $CurationRecoverableError" in new TestCase {

      val commitEvents  = commitsLists(size = Gen.const(1)).generateOne
      val commit +: Nil = commitEvents.toList

      givenFetchingAccessToken(forProjectId = commitEvents.head.project.id)
        .returning(context.pure(maybeAccessToken))

      val rawTriples = jsonLDTriples.generateOne
      (triplesFinder
        .generateTriples(_: CommitEvent)(_: Option[AccessToken]))
        .expects(commit, maybeAccessToken)
        .returning(rightT[Try, ProcessingRecoverableError](rawTriples))

      val exception = CurationRecoverableError(nonBlankStrings().generateOne.value, exceptions.generateOne)
      (triplesCurator
        .curate(_: CommitEvent, _: JsonLDTriples)(_: Option[AccessToken]))
        .expects(commit, rawTriples, maybeAccessToken)
        .returning(leftT[Try, CuratedTriples](exception))

      expectEventMarkedAsRecoverableFailure(commit.compoundEventId, exception)

      eventProcessor.process(eventId, commitEvents) shouldBe context.unit

      logError(commitEvents.head, exception.getMessage)
      logSummary(commitEvents, uploaded = 0, failed = 1)
    }

    "succeed and mark event with NonRecoverableFailure if curating triples fails" in new TestCase {

      val commitEvents  = commitsLists(size = Gen.const(1)).generateOne
      val commit +: Nil = commitEvents.toList

      givenFetchingAccessToken(forProjectId = commitEvents.head.project.id)
        .returning(context.pure(maybeAccessToken))

      val rawTriples = jsonLDTriples.generateOne
      (triplesFinder
        .generateTriples(_: CommitEvent)(_: Option[AccessToken]))
        .expects(commit, maybeAccessToken)
        .returning(rightT[Try, ProcessingRecoverableError](rawTriples))

      val exception = exceptions.generateOne
      (triplesCurator
        .curate(_: CommitEvent, _: JsonLDTriples)(_: Option[AccessToken]))
        .expects(commit, rawTriples, maybeAccessToken)
        .returning(EitherT.liftF[Try, ProcessingRecoverableError, CuratedTriples](context.raiseError(exception)))

      expectEventMarkedAsNonRecoverableFailure(commit.compoundEventId, exception)

      eventProcessor.process(eventId, commitEvents) shouldBe context.unit

      logError(commitEvents.head, exception)
      logSummary(commitEvents, uploaded = 0, failed = 1)
    }

    "succeed and mark event with RecoverableFailure " +
      s"if uploading triples to the dataset fails with $DeliveryFailure for at least one event" in new TestCase {

      val commitEvents              = commitsLists(size = Gen.const(2)).generateOne
      val commit1 +: commit2 +: Nil = commitEvents.toList

      givenFetchingAccessToken(forProjectId = commitEvents.head.project.id)
        .returning(context.pure(maybeAccessToken))

      val rawTriples = jsonLDTriples.generateOne
      (triplesFinder
        .generateTriples(_: CommitEvent)(_: Option[AccessToken]))
        .expects(commit1, maybeAccessToken)
        .returning(rightT[Try, ProcessingRecoverableError](rawTriples))

      val curatedTriples = curatedTriplesObjects.generateOne
      (triplesCurator
        .curate(_: CommitEvent, _: JsonLDTriples)(_: Option[AccessToken]))
        .expects(commit1, rawTriples, maybeAccessToken)
        .returning(rightT[Try, ProcessingRecoverableError](curatedTriples))

      val uploadingError = nonEmptyStrings().map(DeliveryFailure.apply).generateOne
      (uploader
        .upload(_: CuratedTriples))
        .expects(curatedTriples)
        .returning(context.pure(uploadingError))

      val exception2 = exceptions.generateOne
      (triplesFinder
        .generateTriples(_: CommitEvent)(_: Option[AccessToken]))
        .expects(commit2, maybeAccessToken)
        .returning(EitherT.liftF[Try, ProcessingRecoverableError, JsonLDTriples](context.raiseError(exception2)))

      expectEventMarkedAsRecoverableFailure(commit1.compoundEventId, uploadingError)

      eventProcessor.process(eventId, commitEvents) shouldBe context.unit

      logError(commitEvents.head, uploadingError.message)
      logSummary(commitEvents, uploaded = 0, failed = 2)
    }

    "succeed and mark event with NonRecoverableFailure " +
      s"if uploading triples to the store fails with either $InvalidTriplesFailure or $InvalidUpdatesFailure for at least one event" in new TestCase {

      (InvalidTriplesFailure("error") +: InvalidUpdatesFailure("error") +: Nil) foreach { failure =>
        val commitEvents              = commitsLists(size = Gen.const(2)).generateOne
        val commit1 +: commit2 +: Nil = commitEvents.toList

        givenFetchingAccessToken(forProjectId = commitEvents.head.project.id)
          .returning(context.pure(maybeAccessToken))

        val rawTriples = jsonLDTriples.generateOne
        (triplesFinder
          .generateTriples(_: CommitEvent)(_: Option[AccessToken]))
          .expects(commit1, maybeAccessToken)
          .returning(rightT[Try, ProcessingRecoverableError](rawTriples))

        val curatedTriples = curatedTriplesObjects.generateOne
        (triplesCurator
          .curate(_: CommitEvent, _: JsonLDTriples)(_: Option[AccessToken]))
          .expects(commit1, rawTriples, maybeAccessToken)
          .returning(rightT[Try, ProcessingRecoverableError](curatedTriples))

        (uploader
          .upload(_: CuratedTriples))
          .expects(curatedTriples)
          .returning(context.pure(failure))

        val exception2 = exceptions.generateOne
        (triplesFinder
          .generateTriples(_: CommitEvent)(_: Option[AccessToken]))
          .expects(commit2, maybeAccessToken)
          .returning(EitherT.liftF[Try, ProcessingRecoverableError, JsonLDTriples](context.raiseError(exception2)))

        expectEventMarkedAsNonRecoverableFailure(commit1.compoundEventId, failure)

        eventProcessor.process(eventId, commitEvents) shouldBe context.unit

        logError(commitEvents.head, failure.getMessage)
        logSummary(commitEvents, uploaded = 0, failed = 2)
        logger.reset()
      }
    }

    "succeed and log an error if marking event as TriplesStore fails" in new TestCase {

      val commitEvents  = commitsLists(size = Gen.const(1)).generateOne
      val commit +: Nil = commitEvents.toList

      givenFetchingAccessToken(forProjectId = commitEvents.head.project.id)
        .returning(context.pure(maybeAccessToken))

      successfulTriplesGenerationAndUpload(commit -> jsonLDTriples.generateOne)

      val exception = exceptions.generateOne
      (eventStatusUpdater
        .markEventDone(_: CompoundEventId))
        .expects(commit.compoundEventId)
        .returning(context.raiseError(exception))

      eventProcessor.process(eventId, commitEvents) shouldBe context.unit

      logError(commitEvents.head, exception, "failed to mark as TriplesStore in the Event Log")
      logSummary(commitEvents, uploaded = 1, failed = 0)
    }

    "mark event as New and log an error if finding an access token fails" in new TestCase {

      val commitEvents = commitsLists(size = Gen.const(1)).generateOne

      val exception = exceptions.generateOne
      givenFetchingAccessToken(forProjectId = commitEvents.head.project.id)
        .returning(context.raiseError(exception))

      (eventStatusUpdater
        .markEventNew(_: CompoundEventId))
        .expects(commitEvents.head.compoundEventId)
        .returning(context.unit)

      eventProcessor.process(eventId, commitEvents) shouldBe context.unit

      logger.loggedOnly(
        Error(
          message          = s"Commit Event processing failure: $eventId, projectPath: ${commitEvents.head.project.path}",
          throwableMatcher = NotRefEqual(new Exception("processing failure -> Event rolled back", exception))
        )
      )
    }
  }

  "eventsProcessingTimes histogram" should {

    "have 'events_processing_times' name" in {
      eventsProcessingTimes.startTimer().observeDuration()

      eventsProcessingTimes.collect().asScala.headOption.map(_.name) shouldBe Some(
        "events_processing_times"
      )
    }

    "be registered in the Metrics Registry" in {

      val metricsRegistry = mock[MetricsRegistry[IO]]

      (metricsRegistry
        .register[Histogram, Histogram.Builder](_: Histogram.Builder)(_: MonadError[IO, Throwable]))
        .expects(eventsProcessingTimesBuilder, *)
        .returning(IO.pure(eventsProcessingTimes))

      val logger = TestLogger[IO]()
      IOCommitEventProcessor(
        mock[TriplesGenerator[IO]],
        metricsRegistry,
        Throttler.noThrottling,
        new SparqlQueryTimeRecorder(TestExecutionTimeRecorder(logger)),
        logger
      ).unsafeRunSync()
    }
  }

  private implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  private implicit val timer:        Timer[IO]        = IO.timer(ExecutionContext.global)
  private lazy val eventsProcessingTimes = eventsProcessingTimesBuilder.create()

  private trait TestCase {
    val context = MonadError[Try, Throwable]

    val eventId          = compoundEventIds.generateOne
    val maybeAccessToken = Gen.option(accessTokens).generateOne

    val accessTokenFinder     = mock[AccessTokenFinder[Try]]
    val triplesFinder         = mock[TriplesGenerator[Try]]
    val triplesCurator        = mock[TryTriplesCurator]
    val uploader              = mock[TryUploader]
    val eventStatusUpdater    = mock[EventStatusUpdater[Try]]
    val logger                = TestLogger[Try]()
    val executionTimeRecorder = TestExecutionTimeRecorder[Try](logger, Option(eventsProcessingTimes))
    val eventProcessor = new CommitEventProcessor(
      accessTokenFinder,
      triplesFinder,
      triplesCurator,
      uploader,
      eventStatusUpdater,
      logger,
      executionTimeRecorder
    )

    def givenFetchingAccessToken(forProjectId: Id) =
      (accessTokenFinder
        .findAccessToken(_: Id)(_: Id => String))
        .expects(forProjectId, projectIdToPath)

    def generateTriples(forCommits: NonEmptyList[CommitEvent]): NonEmptyList[(CommitEvent, JsonLDTriples)] =
      forCommits map (_ -> jsonLDTriples.generateOne)

    def successfulTriplesGenerationAndUpload(commitAndTriples: (CommitEvent, JsonLDTriples)) = {
      val (commit, triples) = commitAndTriples
      (triplesFinder
        .generateTriples(_: CommitEvent)(_: Option[AccessToken]))
        .expects(commit, maybeAccessToken)
        .returning(rightT[Try, ProcessingRecoverableError](triples))

      val curatedTriples = curatedTriplesObjects.generateOne
      (triplesCurator
        .curate(_: CommitEvent, _: JsonLDTriples)(_: Option[AccessToken]))
        .expects(commit, triples, maybeAccessToken)
        .returning(rightT[Try, ProcessingRecoverableError](curatedTriples))

      (uploader
        .upload(_: CuratedTriples))
        .expects(curatedTriples)
        .returning(context.pure(DeliverySuccess))
    }

    def expectEventMarkedDone(commitEventId: CompoundEventId) =
      (eventStatusUpdater
        .markEventDone(_: CompoundEventId))
        .expects(commitEventId)
        .returning(context.unit)

    def expectEventMarkedAsRecoverableFailure(commitEventId: CompoundEventId, exception: Throwable) =
      (eventStatusUpdater
        .markEventFailedRecoverably(_: CompoundEventId, _: Throwable))
        .expects(commitEventId, exception)
        .returning(context.unit)

    def expectEventMarkedAsNonRecoverableFailure(commitEventId: CompoundEventId, exception: Throwable) =
      (eventStatusUpdater
        .markEventFailedNonRecoverably(_: CompoundEventId, _: Throwable))
        .expects(commitEventId, exception)
        .returning(context.unit)

    def logSummary(commits: NonEmptyList[CommitEvent], uploaded: Int, failed: Int): Assertion =
      logger.logged(
        Info(
          s"${commonLogMessage(commits.head)} processed in ${executionTimeRecorder.elapsedTime}ms: " +
            s"${commits.size} commits, $uploaded commits uploaded, $failed commits failed"
        )
      )

    def logError(commit: CommitEvent, message: String): Assertion =
      logger.logged(Error(s"${commonLogMessage(commit)} $message"))

    def logError(commit: CommitEvent, exception: Exception, message: String = "failed"): Assertion =
      logger.logged(Error(s"${commonLogMessage(commit)} $message", exception))

    def commonLogMessage(event: CommitEvent): String =
      s"Commit Event id: ${event.compoundEventId}, ${event.project.path}"

    def verifyMetricsCollected() =
      eventsProcessingTimes
        .collect()
        .asScala
        .flatMap(_.samples.asScala.map(_.name))
        .exists(_ startsWith "events_processing_times") shouldBe true
  }

  private def commits(commitId: CommitId, project: Project): Gen[CommitEvent] =
    for {
      maybeParentId <- Gen.option(commitIds)
    } yield maybeParentId match {
      case None           => CommitEventWithoutParent(EventId(commitId.value), project, commitId)
      case Some(parentId) => CommitEventWithParent(EventId(commitId.value), project, commitId, parentId)
    }

  private def commitsLists(size: Gen[Int Refined Positive] = positiveInts(max = 5)): Gen[NonEmptyList[CommitEvent]] =
    for {
      commitId <- commitIds
      project  <- projects
      size     <- size
      commits  <- Gen.listOfN(size.value, commits(commitId, project))
    } yield NonEmptyList.fromListUnsafe(commits)
}
