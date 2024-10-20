package core

import cats.data.*
import core.*
import scala.collection.immutable.ListSet
import java.util.Collection
import java.time.LocalDateTime

enum KanbanBoardValidationError:
  case DuplicateFound extends KanbanBoardValidationError
  case TaskNotFoundInColumn(column: KanbanBoardColumn, task: Task)
      extends KanbanBoardValidationError
end KanbanBoardValidationError

object KanbanBoardValidationError:
  def validateDuplicates(
      state: KanbanBoard
  ): ValidatedNel[KanbanBoardValidationError, KanbanBoard] =
    state match {
      case KanbanBoard(
            backLog,
            inProgress,
            peerReview,
            inTest,
            done,
            blocked
          ) => {
        val result = for {
          acc <- Option.when(
            (backLog.toListSet & inProgress.toListSet).isEmpty
          )(backLog.toListSet ++ inProgress.toListSet)
          acc <- Option.when((acc & peerReview.toListSet).isEmpty)(
            acc ++ peerReview.toListSet
          )
          acc <- Option.when((acc & inTest.toListSet).isEmpty)(
            acc ++ inTest.toListSet
          )
          acc <- Option.when((acc & done.toListSet).isEmpty)(
            acc ++ done.toListSet
          )
          acc <- Option.when((acc & blocked.toListSet).isEmpty)(
            acc ++ blocked.toListSet
          )
        } yield state

        Validated.fromOption(
          result,
          NonEmptyList.one(KanbanBoardValidationError.DuplicateFound)
        )
      }
    }
end KanbanBoardValidationError

final case class KanbanBoard(
    backLog: BackLog,
    inProgress: InProgress,
    peerReview: PeerReview,
    inTest: InTest,
    done: Done,
    blocked: Blocked
)

object KanbanBoard:
  private def emptyList(): ListSet[Task] = ListSet()

  def apply(
      backLog: BackLog,
      inProgress: InProgress,
      peerReview: PeerReview,
      inTest: InTest,
      done: Done,
      blocked: Blocked
  ): ValidatedNel[
    KanbanBoardValidationError,
    KanbanBoard
  ] =
    KanbanBoardValidationError.validateDuplicates(
      new KanbanBoard(
        backLog: BackLog,
        inProgress: InProgress,
        peerReview: PeerReview,
        inTest: InTest,
        done: Done,
        blocked: Blocked
      )
    )

  def apply(): ValidatedNel[KanbanBoardValidationError, KanbanBoard] =
    Validated.valid(
      new KanbanBoard(
        BackLog(),
        InProgress(),
        PeerReview(),
        InTest(),
        Done(),
        Blocked()
      )
    )

  def removeTask(
      from: KanbanBoardColumn,
      task: Task
  ): State[KanbanBoard, Option[Task]] =
    from match
      case KanbanBoardColumn.BackLog =>
        State(state =>
          (
            state.copy(backLog = state.backLog - task),
            state.backLog.toListSet
              .find(_ == task)
              .map(_.copy(status = KanbanBoardColumn.BackLog))
          )
        )
      case KanbanBoardColumn.InProgress =>
        State(state =>
          (
            state.copy(inProgress = state.inProgress - task),
            state.inProgress.toListSet
              .find(_ == task)
              .map(_.copy(status = KanbanBoardColumn.InProgress))
          )
        )
      case KanbanBoardColumn.PeerReview =>
        State(state =>
          (
            state.copy(peerReview = state.peerReview - task),
            state.peerReview.toListSet
              .find(_ == task)
              .map(_.copy(status = KanbanBoardColumn.PeerReview))
          )
        )
      case KanbanBoardColumn.InTest =>
        State(state =>
          (
            state.copy(inTest = state.inTest - task),
            state.inTest.toListSet
              .find(_ == task)
              .map(_.copy(status = KanbanBoardColumn.InTest))
          )
        )
      case KanbanBoardColumn.Done =>
        State(state =>
          (
            state.copy(done = state.done - task),
            state.done.toListSet
              .find(_ == task)
              .map(_.copy(status = KanbanBoardColumn.Done))
          )
        )
      case KanbanBoardColumn.Blocked =>
        State(state =>
          (
            state.copy(blocked = state.blocked - task),
            state.blocked.toListSet
              .find(_ == task)
              .map(_.copy(status = KanbanBoardColumn.Blocked))
          )
        )

  def addTask(
      to: KanbanBoardColumn,
      task: Task
  ): State[KanbanBoard, Option[Task]] =
    to match
      case KanbanBoardColumn.BackLog =>
        State(state =>
          (
            state.copy(backLog = state.backLog + task),
            state.backLog.toListSet
              .find(_ == task)
              .map(_.copy(status = KanbanBoardColumn.BackLog))
          )
        )
      case KanbanBoardColumn.InProgress =>
        State(state =>
          (
            state.copy(inProgress = state.inProgress + task),
            state.inProgress.toListSet
              .find(_ == task)
              .map(_.copy(status = KanbanBoardColumn.InProgress))
          )
        )
      case KanbanBoardColumn.PeerReview =>
        State(state =>
          (
            state.copy(peerReview = state.peerReview + task),
            state.peerReview.toListSet
              .find(_ == task)
              .map(_.copy(status = KanbanBoardColumn.PeerReview))
          )
        )
      case KanbanBoardColumn.InTest =>
        State(state =>
          (
            state.copy(inTest = state.inTest + task),
            state.inTest.toListSet
              .find(_ == task)
              .map(_.copy(status = KanbanBoardColumn.InTest))
          )
        )
      case KanbanBoardColumn.Done =>
        State(state =>
          (
            state.copy(done = state.done + task),
            state.done.toListSet
              .find(_ == task)
              .map(_.copy(status = KanbanBoardColumn.Done))
          )
        )
      case KanbanBoardColumn.Blocked =>
        State(state =>
          (
            state.copy(blocked = state.blocked + task),
            state.blocked.toListSet
              .find(_ == task)
              .map(_.copy(status = KanbanBoardColumn.Blocked))
          )
        )

  def moveTask(
      from: KanbanBoardColumn,
      to: KanbanBoardColumn,
      task: Task
  ): State[KanbanBoard, Option[Task]] = for {
    removedTask <- removeTask(from, task)
    addedTask <- addTask(to, task)
  } yield addedTask

end KanbanBoard
