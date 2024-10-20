package core

import java.util.UUID
import java.time.LocalDateTime

opaque type TaskId = Int
object TaskId:
  def of(value: Int): TaskId = value
end TaskId

final case class Task(
    id: TaskId,
    uuid: UUID,
    name: String,
    description: String,
    status: KanbanBoardColumn
) {
  def apply(id: TaskId, uuid: UUID, name: String, description: String) =
    Task(id, uuid, name, description, status = KanbanBoardColumn.BackLog)

  def canEqual(a: Any) = a.isInstanceOf[Task]

  override def equals(that: Any): Boolean =
    that match {
      case that: Task => {
        that.canEqual(this) &&
        this.id == that.id
      }
      case _ => false
    }
}
