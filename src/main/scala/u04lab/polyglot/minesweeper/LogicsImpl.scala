package u04lab.polyglot.minesweeper

import u04lab.polyglot.minesweeper.Pair
import u04lab.code.Option.*
import u04lab.code.Stream.{filter, forEach, toList}
import u04lab.code.List
import u04lab.polyglot.minesweeper.Status.{Flagged, Hidden, Revealed}

class LogicsImpl(gridSize: Int, numberOfMines: Int) extends Logics:

  private val grid: Grid = Grid(gridSize, numberOfMines)

  override def getNeighbourMines(cellPosition: Pair[Integer, Integer]): Int = adjacentCellsTo(cellPosition)

  override def hasFlag(cellPosition: Pair[Integer, Integer]): Boolean = cellAt(cellPosition).status == Flagged

  override def hasMine(cellPosition: Pair[Integer, Integer]): Boolean = cellAt(cellPosition).hasMine

  override def isRevealed(cellPosition: Pair[Integer, Integer]): Boolean = cellAt(cellPosition).status == Revealed

  override def toggleFlag(cellPosition: Pair[Integer, Integer]): Unit = cellAt(cellPosition).status match
    case Hidden => cellAt(cellPosition).status = Flagged
    case Flagged => cellAt(cellPosition).status = Hidden

  override def won: Boolean = List.length(toList(filter(grid.cells)(_.status == Hidden))) == numberOfMines

  override def hit(cellPosition: Pair[Integer, Integer]): Boolean =
    val hitMine = hasMine(cellPosition)
    if !hitMine then
      val hitCell = cellAt(cellPosition)
      hitCell.status = Revealed
      revealAdjacentRecursively(hitCell)
    hitMine

  private def revealAdjacentRecursively(cell: Cell): Unit =
    forEach(filter(grid.adjacentCellsTo(cell))(c => c.status != Revealed && !c.hasMine)) { c =>
      c.status = Revealed
      if adjacentCellsTo(c.position) == 0 then revealAdjacentRecursively(c)
    }

  private def cellAt(cellPosition: Pair[Integer, Integer]): Cell =
    grid.cellAt(Position(cellPosition)) match { case Some(x) => x }

  private def adjacentCellsTo(cellPosition: Pair[Integer, Integer]): Int =
    grid.adjacentMinesTo(Position(cellPosition)) match { case Some(x) => x }
