package aoc24.d4

import com.malex1337.Day


class Day4(fileName: String) extends Day(fileName) {
  private val directions = Array(Point(0, -1), Point(1, -1), Point(1, 0), Point(1, 1), Point(0, 1), Point(-1, 1),
    Point(-1, 0), Point(-1, -1))
  private val grid = lines.map(_.toCharArray)
  private val xmas = Array('X', 'M', 'A', 'S')

  override protected def part1(): Unit = {
    var amnt = 0

    findAll('X').foreach(x => amnt += directions.map(d => parse(x, 1, d)).sum)

    println(s"Part 1: $amnt")
  }

  override protected def part2(): Unit = {
    val amnt = findAll('A').count(a => {
      val tlBr = inGrid(a.x - 1, a.y - 1) && inGrid(a.x + 1, a.y + 1)
        && ((grid(a.y - 1)(a.x - 1) == 'M' && grid(a.y + 1)(a.x + 1) == 'S')
        || (grid(a.y - 1)(a.x - 1) == 'S' && grid(a.y + 1)(a.x + 1) == 'M'))

      val trBl = inGrid(a.x + 1, a.y - 1) && inGrid(a.x - 1, a.y + 1)
        && ((grid(a.y - 1)(a.x + 1) == 'M' && grid(a.y + 1)(a.x - 1) == 'S')
        || (grid(a.y - 1)(a.x + 1) == 'S' && grid(a.y + 1)(a.x - 1) == 'M'))
      tlBr && trBl
    })

    println(s"Part 2: $amnt")
  }

  private def parse(point: Point, idx: Int, direction: Point): Int = {
    if (idx == xmas.length) {
      return 1
    }

    val nextPoint = Point(point.x + direction.x, point.y + direction.y)
    if (inGrid(nextPoint) && grid(nextPoint.y)(nextPoint.x) == xmas(idx)) {
      parse(nextPoint, idx + 1, direction)
    } else {
      0
    }
  }

  private def inGrid(p: Point): Boolean = {
    inGrid(p.x, p.y)
  }

  private def inGrid(x: Int, y: Int): Boolean = {
    y >= 0 && y < grid.length
      && x >= 0 && x < grid(0).length
  }

  private def findAll(c: Char): List[Point] = {
    for {
      (row, rowIndex) <- grid.zipWithIndex
      (char, colIndex) <- row.zipWithIndex
      if char == c
    } yield Point(colIndex, rowIndex)
  }.toList
}

class Point(val x: Integer, val y: Integer)

object Day4 {
  def main(args: Array[String]): Unit = {
    Day4("/aoc24_d4").runAll()
  }
}