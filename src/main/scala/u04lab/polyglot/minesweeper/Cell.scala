package u04lab.polyglot.minesweeper

enum Status:
  case Hidden
  case Revealed
  case Flagged

trait Cell:

  def position: Position

  def hasMine: Boolean

  def isAdjacentTo(cell: Cell): Boolean

  def status: Status

  def status_=(newStatus: Status): Unit


object Cell:
  import Status.*

  def apply(position: Position): Cell = CellImpl(position = position, hasMine = false, _status = Hidden)

  def CellWithMine(position: Position): Cell = CellImpl(position = position, hasMine = true, _status = Hidden)

  private case class CellImpl(position: Position, hasMine: Boolean, private var _status: Status) extends Cell:
    override def isAdjacentTo(cell: Cell): Boolean =
      cell.position != position && Math.abs(cell.position.row - position.row) <= 1 &&
        Math.abs(cell.position.column - position.column) <= 1

    override def status_=(newStatus: Status): Unit = if _status != Revealed then _status = newStatus

    override def status: Status = _status
