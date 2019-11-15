package example

import java.lang.Math.abs

import scala.util.chaining.scalaUtilChainingOps

case class World(cells: Seq[Cell]) {
  def getNextGen =
    cells
      .flatMap(surroundingNeighbourCells)
      .pipe(calculateCellScores)
      .filter(gc => gc._2 == 3 || (cells.contains(gc._1) && Seq(2, 3).contains(getNeighbourCount(gc._1))))
      .map(_._1)
    .pipe(World)

  def calculateCellScores(cells: Seq[Cell]) = cells.groupBy(identity).view.mapValues(_.length).toSeq

  def surroundingNeighbourCells(cell: Cell) =
    (cell.x - 1 to cell.x + 1)
      .flatMap(x => (cell.y - 1 to cell.y + 1)
        .map(y => Cell(x, y)))

  def getNeighbourCount(cell: Cell) =
    cells
      .filterNot(_ == cell)
      .count(c => areNeighbours(cell.x, c.x) && areNeighbours(cell.y, c.y) )

  def areNeighbours(p1: Int, p2: Int) = abs(p1 - p2) <= 1
}

case class Cell(x: Int, y: Int)
