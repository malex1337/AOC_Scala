package aoc24.d6

import com.malex1337.Day

import scala.annotation.tailrec


class Day6(fileName: String) extends Day(fileName) {

  override protected def part1(): Unit = {
    val grid = lines.map(l => l.toCharArray).toArray
    val position = grid.zipWithIndex.flatMap { case (row, rowIndex) =>
      row.zipWithIndex.collect {
        case (char, colIndex) if char == '^' => Pos(colIndex, rowIndex)
      }
    }.head
    grid(position.y)(position.x) = '.'

    val uniquePositions = walk(grid, position, Direction(), Set(position))

    println(s"Part 1: ${uniquePositions.size}")
  }

  override protected def part2(): Unit = {

  }

  @tailrec private def walk(grid: Array[Array[Char]], pos: Pos, dir: Direction, visited: Set[Pos]): Set[Pos] = {
    val updated = if grid(pos.y)(pos.x) == '.' then visited + pos else visited
    val newPos = pos.walk(dir)

    charAt(grid, newPos) match
      case Some('.') => walk(grid, newPos, dir, updated)
      case Some(_) => walk(grid, pos, dir.turnRight(), updated)
      case None => updated
  }

  def charAt(grid: Array[Array[Char]], pos: Pos): Option[Char] = {
    if pos.x >= 0 && pos.y >= 0 && pos.x < grid(0).length && pos.y < grid.length then Some(grid(pos.y)(pos.x)) else None
  }

  case class Direction(var x: Int = 0, var y: Int = -1) {
    def turnRight(): Direction = {
      (x, y) match {
        case (0, -1) => Direction(1, 0) // UP > RIGHT
        case (1, 0) => Direction(0, 1) // RIGHT > DOWN
        case (0, 1) => Direction(-1, 0) // DOWN > LEFT
        case (-1, 0) => Direction(0, -1) // UP
      }
    }
  }

  case class Pos(var x: Int, var y: Int) {
    def walk(direction: Direction): Pos = Pos(x + direction.x, y + direction.y)
  }
}

object Day6 {
  def main(args: Array[String]): Unit = {
    Day6("aoc24_d6").runAll()
  }
}