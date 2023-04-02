package u04lab.polyglot.minesweeper

import u04lab.polyglot.minesweeper.Pair
import u04lab.code.Option.*
import u04lab.code.Stream.{filter, forEach, toList}
import u04lab.code.List
import u04lab.polyglot.minesweeper.Status.{Flagged, Hidden, Revealed}

class LogicsImpl(gridSize: Int, numberOfMines: Int) extends Logics:

  private val grid: Grid = Grid(gridSize, numberOfMines)

  override def getNeighbourMines(cellPosition: Pair[Integer, Integer]): Int = cellAt(cellPosition)._2

  override def hasFlag(cellPosition: Pair[Integer, Integer]): Boolean = cellAt(cellPosition)._1.status == Flagged

  override def hasMine(cellPosition: Pair[Integer, Integer]): Boolean = cellAt(cellPosition)._1.hasMine

  override def isRevealed(cellPosition: Pair[Integer, Integer]): Boolean = cellAt(cellPosition)._1.status == Revealed

  override def toggleFlag(cellPosition: Pair[Integer, Integer]): Unit =
    cellAt(cellPosition) match { case (c, _) => c.status = if c.status == Hidden then Flagged else Hidden }

  override def won: Boolean = List.length(toList(filter(grid.cells)(c => c.status == Hidden))) == numberOfMines

  override def hit(cellPosition: Pair[Integer, Integer]): Boolean =
    val hitMine = hasMine(cellPosition)
    if !hitMine then
      val hitCell = cellAt(cellPosition)._1
      hitCell.status = Revealed
      revealAdjacentRecursively(hitCell)
    hitMine

  private def revealAdjacentRecursively(cell: Cell): Unit =
    forEach(filter(grid.adjacentCellsTo(cell))((c, _) => c.status != Revealed && !c.hasMine)) { (c, i) =>
      c.status = Revealed
      if i == 0 then revealAdjacentRecursively(c)
    }

  private def cellAt(cellPosition: Pair[Integer, Integer]): (Cell, Int) =
    grid.cellAt(Position(cellPosition)) match { case Some((c, i)) => (c, i) }
