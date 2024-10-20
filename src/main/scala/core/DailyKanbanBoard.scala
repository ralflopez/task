package core

import java.time.LocalDateTime
import cats.data.ValidatedNel

final case class DailyKanbanBoard(
    startTime: LocalDateTime,
    endTime: Option[LocalDateTime],
    kanbanBoard: KanbanBoard
)

object DailyKanbanBoard:
  def apply(
      startTime: LocalDateTime
  ): ValidatedNel[KanbanBoardValidationError, DailyKanbanBoard] = {
    KanbanBoard().map(kanbanBoard =>
      new DailyKanbanBoard(
        startTime = startTime,
        endTime = None,
        kanbanBoard = kanbanBoard
      )
    )
  }
end DailyKanbanBoard
