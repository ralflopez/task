package core

import java.util.UUID
import scala.collection.immutable.ListSet
import core.*
import cats.data.*

class KanbanBoardTest extends munit.FunSuite {
  private def createTask(taskId: Int) = Task(
    id = TaskId.of(taskId),
    description = "description",
    name = "name",
    uuid = UUID.randomUUID(),
    status = KanbanBoardColumn.BackLog
  )

  test("should compare equality by column content") {
    val base = KanbanBoard(
      BackLog(createTask(1), createTask(2)),
      InProgress(createTask(3)),
      PeerReview(createTask(4), createTask(5), createTask(6)),
      InTest(),
      Done(createTask(7)),
      Blocked(createTask(8), createTask(9))
    )

    val sameContent = KanbanBoard(
      BackLog(createTask(1), createTask(2)),
      InProgress(createTask(3)),
      PeerReview(createTask(4), createTask(5), createTask(6)),
      InTest(),
      Done(createTask(7)),
      Blocked(createTask(8), createTask(9))
    )

    val sameContentDifferentOrder = KanbanBoard(
      BackLog(createTask(1), createTask(2)),
      InProgress(createTask(3)),
      PeerReview(createTask(6), createTask(4), createTask(5)),
      InTest(),
      Done(createTask(7)),
      Blocked(createTask(9), createTask(8))
    )

    val sameCardinalityDifferentContent = KanbanBoard(
      BackLog(createTask(3), createTask(2)),
      InProgress(createTask(3)),
      PeerReview(createTask(6), createTask(4), createTask(5)),
      InTest(),
      Done(createTask(7)),
      Blocked(createTask(9), createTask(8))
    )

    val differentCardinality = KanbanBoard(
      BackLog(createTask(2)),
      InProgress(createTask(3)),
      PeerReview(createTask(6), createTask(4), createTask(5)),
      InTest(),
      Done(createTask(7)),
      Blocked(createTask(9), createTask(8))
    )

    assertEquals(base, sameContent)
    assertEquals(base, sameContentDifferentOrder)
    assertNotEquals(base, sameCardinalityDifferentContent)
    assertNotEquals(base, differentCardinality)
  }

  test(
    "should identify as valid state if all the tasks are unique"
  ) {
    val kanbanBoard = KanbanBoard(
      backLog = BackLog(
        createTask(1),
        createTask(2)
      ),
      inProgress = InProgress(
        createTask(3),
        createTask(4)
      ),
      peerReview = PeerReview(
        createTask(5),
        createTask(6)
      ),
      inTest = InTest(
        createTask(7),
        createTask(8)
      ),
      done = Done(
        createTask(9),
        createTask(10)
      ),
      blocked = Blocked(
        createTask(11),
        createTask(12)
      )
    )

    assert(kanbanBoard.isValid)
  }

  test(
    "should identify as invalid state if there are 2 of the same task in different columns"
  ) {
    val kanbanBoard = KanbanBoard(
      backLog = BackLog(
        createTask(1),
        createTask(2)
      ),
      inProgress = InProgress(),
      peerReview = PeerReview(),
      inTest = InTest(),
      done = Done(
        createTask(1)
      ),
      blocked = Blocked()
    )

    assert(kanbanBoard.isInvalid)
  }

  test("should remove tasks from columns") {
    val process: State[KanbanBoard, Option[Task]] = for {
      _ <- KanbanBoard
        .removeTask(KanbanBoardColumn.BackLog, createTask(1))
      _ <- KanbanBoard
        .removeTask(KanbanBoardColumn.InProgress, createTask(3))
      _ <- KanbanBoard
        .removeTask(KanbanBoardColumn.PeerReview, createTask(6))
      _ <- KanbanBoard
        .removeTask(KanbanBoardColumn.Done, createTask(9))
      task <- KanbanBoard
        .removeTask(KanbanBoardColumn.Blocked, createTask(11))
    } yield task

    val result = KanbanBoard(
      BackLog(createTask(1), createTask(2)),
      InProgress(createTask(3)),
      PeerReview(createTask(5), createTask(6)),
      InTest(),
      Done(createTask(9), createTask(10)),
      Blocked(createTask(11), createTask(12))
    ).map(process.run(_).value)

    val resultState = result.map((state, _) => state)

    val expected = KanbanBoard(
      BackLog(createTask(2)),
      InProgress(),
      PeerReview(createTask(5)),
      InTest(),
      Done(createTask(10)),
      Blocked(createTask(12))
    )

    assertEquals(expected, resultState)
  }

  test("should add tasks to columns") {
    val process: State[KanbanBoard, Unit] = for {
      _ <- KanbanBoard
        .addTask(KanbanBoardColumn.BackLog, createTask(1))
      _ <- KanbanBoard
        .addTask(KanbanBoardColumn.BackLog, createTask(2))
      _ <- KanbanBoard
        .addTask(KanbanBoardColumn.InProgress, createTask(3))
      _ <- KanbanBoard
        .addTask(KanbanBoardColumn.PeerReview, createTask(5))
      _ <- KanbanBoard
        .addTask(KanbanBoardColumn.InTest, createTask(7))
      _ <- KanbanBoard
        .addTask(KanbanBoardColumn.Done, createTask(9))
      _ <- KanbanBoard
        .addTask(KanbanBoardColumn.Done, createTask(10))
      _ <- KanbanBoard
        .addTask(KanbanBoardColumn.Blocked, createTask(11))
    } yield ()

    val result = KanbanBoard(
      BackLog(),
      InProgress(),
      PeerReview(),
      InTest(),
      Done(),
      Blocked()
    ).map(process.run(_).value)

    val resultState = result.map((state, _) => state)

    val expected = KanbanBoard(
      BackLog(createTask(1), createTask(2)),
      InProgress(createTask(3)),
      PeerReview(createTask(5)),
      InTest(createTask(7)),
      Done(createTask(9), createTask(10)),
      Blocked(createTask(11))
    )

    assertEquals(expected, resultState)
  }

  test("should move task from one column to another") {
    val process: State[KanbanBoard, Unit] = for {
      _ <- KanbanBoard
        .moveTask(
          KanbanBoardColumn.BackLog,
          KanbanBoardColumn.InProgress,
          createTask(1)
        )
      _ <- KanbanBoard
        .moveTask(
          KanbanBoardColumn.BackLog,
          KanbanBoardColumn.InProgress,
          createTask(1)
        )
      _ <- KanbanBoard
        .moveTask(
          KanbanBoardColumn.InProgress,
          KanbanBoardColumn.PeerReview,
          createTask(3)
        )
      _ <- KanbanBoard
        .moveTask(
          KanbanBoardColumn.PeerReview,
          KanbanBoardColumn.InTest,
          createTask(6)
        )
      _ <- KanbanBoard
        .moveTask(
          KanbanBoardColumn.InTest,
          KanbanBoardColumn.Done,
          createTask(6)
        )
      _ <- KanbanBoard
        .moveTask(
          KanbanBoardColumn.Done,
          KanbanBoardColumn.Blocked,
          createTask(10)
        )
      _ <- KanbanBoard
        .moveTask(
          KanbanBoardColumn.Blocked,
          KanbanBoardColumn.BackLog,
          createTask(12)
        )
    } yield ()

    val result = KanbanBoard(
      BackLog(createTask(1), createTask(2)),
      InProgress(createTask(3)),
      PeerReview(createTask(5), createTask(6)),
      InTest(),
      Done(createTask(10)),
      Blocked(createTask(11), createTask(12))
    ).map(process.run(_).value)

    val resultState = result.map((state, _) => state)

    val expected = KanbanBoard(
      BackLog(createTask(2), createTask(12)),
      InProgress(createTask(1)),
      PeerReview(createTask(5), createTask(3)),
      InTest(),
      Done(createTask(6)),
      Blocked(createTask(10), createTask(11))
    )

    assertEquals(expected, resultState)
  }
}
