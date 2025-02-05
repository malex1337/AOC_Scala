package aoc24.d6

import com.malex1337.Day

import scala.annotation.tailrec
import lib.GridExtensions.*
import lib.Pos

class Day6(fileName: String) extends Day(fileName) {
  private var visitedPositions = 0

  override protected def part1(): Unit = {
    val grid = lines.map(l => l.toCharArray).toArray

    val position = grid.findFirst(_ == '^').get
    grid.set(position, '.')

    val uniquePositions = walk(grid, position, Pos.up, Set(position))

    println(s"Part 1: ${uniquePositions.size}")
  }

  override protected def part2(): Unit = {
    var amountLoopingPositions = 0
    val grid = lines.map(l => l.toCharArray).toArray

    val possibleObstacles = grid.findAllMatching(_ != '^')
    val guardPosition = grid.findFirst(_ == '^').get
    grid.set(guardPosition, '.')

    for (possibleObstacle <- possibleObstacles) {
      val gridCopy = grid.copy()
      gridCopy.set(possibleObstacle, '#')
      visitedPositions = 0
      try {
        walk(gridCopy, guardPosition, Pos.up, Set(guardPosition))
      } catch
        case e: LoopingException =>
      if (visitedPositions >= grid.length * grid(0).length + 1)
        amountLoopingPositions += 1

      visitedPositions = 0
    }

    println(s"Amnt of looping positions: $amountLoopingPositions")
  }

  @tailrec private def walk(grid: Array[Array[Char]], pos: Pos, dir: Pos, visited: Set[Pos]): Set[Pos] = {
    if (visitedPositions >= grid.rows() * grid.cols() + 1)
      throw new LoopingException

    val updated = if grid.value(pos) == '.' then visited + pos else visited
    val newPos = pos + dir

    visitedPositions += 1

    charAt(grid, newPos) match
      case Some('.') => walk(grid, newPos, dir, updated)
      case Some(_) => walk(grid, pos, turnRight(dir), updated)
      case None => updated
  }

  def charAt(grid: Array[Array[Char]], pos: Pos): Option[Char] = {
    if grid.validPos(pos) then Some(grid.value(pos)) else None
  }

  private def turnRight(pos: Pos): Pos =
    pos match {
      case Pos.up => Pos.right
      case Pos.right => Pos.down
      case Pos.down => Pos.left
      case Pos.left => Pos.up
    }

  private class LoopingException extends Exception()

}

object Day6 {
  def main(args: Array[String]): Unit = {
    Day6("aoc24_d6").runAll()
    // 5551
    // 1939
  }
}