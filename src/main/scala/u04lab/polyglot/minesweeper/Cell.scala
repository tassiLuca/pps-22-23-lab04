package u04lab.polyglot.minesweeper

enum Status:
  case Hidden
  case Revealed
  case Flagged

trait Cell:

  /**
   * @return the position of this cell.
   */
  def position: Position

  /**
   * @return true if this cell has a mine, false otherwise.
   */
  def hasMine: Boolean

  /**
   * Checks if the given cell is adjacent to this.
   * A cell is not adjacent to itself.
   * @param cell the cell to check against.
   * @return true if is adjacent, false otherwise.
   */
  def isAdjacentTo(cell: Cell): Boolean

  /**
   * @return the [[Status]] of this cell.
   */
  def status: Status

  /**
   * Set the current status of this cell to the given one if it is allowed.
   * @param newStatus the new status of the cell.
   */
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
