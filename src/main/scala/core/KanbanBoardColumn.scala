package core

import scala.collection.immutable.ListSet

enum KanbanBoardColumn:
  case BackLog extends KanbanBoardColumn
  case InProgress extends KanbanBoardColumn
  case PeerReview extends KanbanBoardColumn
  case InTest extends KanbanBoardColumn
  case Done extends KanbanBoardColumn
  case Blocked extends KanbanBoardColumn
end KanbanBoardColumn

opaque type BackLog = ListSet[Task]
object BackLog:
  def apply(values: Task*): BackLog = ListSet[Task](values*)
  def apply(): BackLog = ListSet[Task]()

  extension (value: BackLog)
    def column: KanbanBoardColumn = KanbanBoardColumn.BackLog
    def toListSet: ListSet[Task] = value
    def -(task: Task): BackLog = value - task
    def +(task: Task): BackLog = value + task
  end extension
end BackLog

opaque type InProgress = ListSet[Task]
object InProgress:
  def apply(values: Task*): InProgress = ListSet[Task](values*)
  def apply(): InProgress = ListSet[Task]()
  extension (value: InProgress)
    def column: KanbanBoardColumn = KanbanBoardColumn.InProgress
    def toListSet: ListSet[Task] = value
    def -(task: Task): InProgress = value - task
    def +(task: Task): InProgress = value + task
  end extension
end InProgress

opaque type PeerReview = ListSet[Task]
object PeerReview:
  def apply(values: Task*): PeerReview = ListSet[Task](values*)
  def apply(): PeerReview = ListSet[Task]()
  extension (value: PeerReview)
    def column: KanbanBoardColumn = KanbanBoardColumn.PeerReview
    def toListSet: ListSet[Task] = value
    def -(task: Task): PeerReview = value - task
    def +(task: Task): PeerReview = value + task
  end extension
end PeerReview

opaque type InTest = ListSet[Task]
object InTest:
  def apply(values: Task*): InTest = ListSet[Task](values*)
  def apply(): InTest = ListSet[Task]()
  extension (value: InTest)
    def column: KanbanBoardColumn = KanbanBoardColumn.PeerReview
    def toListSet: ListSet[Task] = value
    def -(task: Task): InTest = value - task
    def +(task: Task): InTest = value + task
  end extension
end InTest

opaque type Done = ListSet[Task]
object Done:
  def apply(values: Task*): Done = ListSet[Task](values*)
  def apply(): Done = ListSet[Task]()
  extension (value: Done)
    def column: KanbanBoardColumn = KanbanBoardColumn.Done
    def toListSet: ListSet[Task] = value
    def -(task: Task): Done = value - task
    def +(task: Task): Done = value + task
  end extension
end Done

opaque type Blocked = ListSet[Task]
object Blocked:
  def apply(values: Task*): Blocked = ListSet[Task](values*)
  def apply(): Blocked = ListSet[Task]()
  extension (value: Blocked)
    def column: KanbanBoardColumn = KanbanBoardColumn.Blocked
    def toListSet: ListSet[Task] = value
    def -(task: Task): Blocked = value - task
    def +(task: Task): Blocked = value + task
  end extension
end Blocked
