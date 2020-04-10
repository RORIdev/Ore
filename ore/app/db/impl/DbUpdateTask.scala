package db.impl

import scala.concurrent.{ExecutionContext, Future}

import play.api.inject.ApplicationLifecycle

import db.impl.access.ProjectBase
import db.impl.query.StatTrackerQueries
import ore.OreConfig
import ore.db.ModelService
import ore.util.OreMDC

import cats.syntax.all._
import com.typesafe.scalalogging
import doobie.`enum`.TransactionIsolation
import doobie.implicits._
import zio.clock.Clock
import zio.{Schedule, RIO, Task, UIO, ZIO, duration}

class DbUpdateTask(config: OreConfig, lifecycle: ApplicationLifecycle, runtime: zio.Runtime[Clock])(
    implicit ec: ExecutionContext,
    projects: ProjectBase[Task],
    service: ModelService[Task]
) {

  val interval: duration.Duration = duration.Duration.fromScala(config.ore.materializedUpdateInterval)

  private val Logger               = scalalogging.Logger.takingImplicit[OreMDC]("DbUpdateTask")
  implicit private val mdc: OreMDC = OreMDC.NoMDC

  Logger.info("DbUpdateTask starting")

  private val materializedViewsSchedule: Schedule[Clock, Any, Int] = Schedule
    .fixed(interval)
    .tapInput(_ => UIO(Logger.debug(s"Updating homepage view")))

  private val statSchedule: Schedule[Clock, Any, Int] =
    Schedule
      .fixed(interval)
      .tapInput(_ => UIO(Logger.debug("Processing stats")))

  private def runningTask(task: RIO[Clock, Unit], schedule: Schedule[Clock, Any, Int]) = {
    val safeTask: ZIO[Clock, Unit, Unit] = task.flatMapError(e => UIO(Logger.error("Running DB task failed", e)))

    runtime.unsafeRun(safeTask.repeat(schedule).fork)
  }

  private val materializedViewsTask = runningTask(
    service.runDbCon(
      sql"SELECT refreshProjectStats()"
        .query[Option[Int]]
        .unique *> sql"REFRESH MATERIALIZED VIEW promoted_versions".update.run.void
    ),
    materializedViewsSchedule
  )

  private def runManyInTransaction(updates: Seq[doobie.Update0]) = {
    import cats.instances.list._
    import doobie._

    service
      .runDbCon(
        for {
          _ <- HC.setTransactionIsolation(TransactionIsolation.TransactionRepeatableRead)
          _ <- updates.toList.traverse_(_.run)
        } yield ()
      )
      .retry(Schedule.forever)
  }

  private val statsTask = runningTask(
    runManyInTransaction(StatTrackerQueries.processProjectViews) *>
      runManyInTransaction(StatTrackerQueries.processVersionDownloads),
    statSchedule
  )
  lifecycle.addStopHook { () =>
    Future {
      runtime.unsafeRun(materializedViewsTask.interrupt)
      runtime.unsafeRun(statsTask.interrupt)
    }
  }
}
