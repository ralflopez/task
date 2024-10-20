package core

import java.util.UUID
import core.Task
import java.util.Collection
import scala.collection.immutable.ListSet

class TaskTest extends munit.FunSuite {
  test("should check equality only by id") {
    val a = Task(
      id = TaskId.of(1),
      description = "any",
      name = "any",
      uuid = UUID.randomUUID(),
      status = KanbanBoardColumn.BackLog
    )

    val b = Task(
      id = TaskId.of(1),
      description = "description",
      name = "name",
      uuid = UUID.randomUUID(),
      status = KanbanBoardColumn.BackLog
    )

    assertEquals(a, b)
  }

  test("should check equality only by id in a list set") {
    val a = ListSet(
      Task(
        id = TaskId.of(2),
        description = "any",
        name = "any",
        uuid = UUID.randomUUID(),
        status = KanbanBoardColumn.BackLog
      ),
      Task(
        id = TaskId.of(1),
        description = "any",
        name = "any",
        uuid = UUID.randomUUID(),
        status = KanbanBoardColumn.BackLog
      )
    )

    val b = ListSet(
      Task(
        id = TaskId.of(1),
        description = "description",
        name = "name",
        uuid = UUID.randomUUID(),
        status = KanbanBoardColumn.BackLog
      ),
      Task(
        id = TaskId.of(2),
        description = "any",
        name = "any",
        uuid = UUID.randomUUID(),
        status = KanbanBoardColumn.BackLog
      )
    )

    assertEquals(a, b)
  }
}
