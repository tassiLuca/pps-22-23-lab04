package u04lab.polyglot.minesweeper

import u04lab.code.Option
import u04lab.code.Stream
import u04lab.code.List
import u04lab.polyglot.minesweeper.Cell.CellWithMine

import scala.annotation.tailrec
import scala.util.Random.nextInt

trait Grid:

  def cellAt(position: Position): Option[(Cell, Int)]

  def adjacentCellsTo(cell: Cell): Stream[(Cell, Int)]

  def cells: Stream[Cell]

object Grid:

  import Stream.*

  def apply(gridSize: Int, numberOfMines: Int): Grid =
    val mines = generateMines(gridSize, numberOfMines, List.empty)
    val cellPositions = List.flatMap(sequenceUntil(gridSize))(x => List.map(sequenceUntil(gridSize))(y => Position(x, y)))
    val cells = List.map(cellPositions)(c => if List.contains(mines, c) then CellWithMine(c) else Cell(c))
    GridImpl(List.toStream(cells))

  private def sequenceUntil(limit: Int): List[Int] = toList(take(iterate(0)(_ + 1))(limit))

  @tailrec
  private def generateMines(gridSize: Int, mines: Int, acc: List[Position]): List[Position] = mines match
    case 0 => acc
    case _ => Position(nextInt(gridSize), nextInt(gridSize)) match
      case x if List.contains(acc, x) => generateMines(gridSize, mines, acc)
      case x => generateMines(gridSize, mines - 1, List.append(acc, List.of(x)))

  private class GridImpl(override val cells: Stream[Cell]) extends Grid:
    private val cellsWithMinesCount: Stream[(Cell, Int)] =
      map(cells)(c => (c, size(filter(cells)(c1 => c1.isAdjacentTo(c) && c1.hasMine))))

    override def adjacentCellsTo(cell: Cell): Stream[(Cell, Int)] =
      filter(cellsWithMinesCount)((c, _) => c.isAdjacentTo(cell))

    override def cellAt(position: Position): Option[(Cell, Int)] =
      List.find(toList(cellsWithMinesCount))((c, _) => c.position == position)
