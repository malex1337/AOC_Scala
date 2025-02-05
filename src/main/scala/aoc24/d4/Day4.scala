package aoc24.d4

import com.malex1337.Day
import lib.GridExtensions.*
import lib.Pos

case class Day4(fileName: String) extends Day(fileName) {
  private val directions = Pos.allDirections :+ Pos(1, -1) :+ Pos(-1, 1) :+ Pos(1, 1) :+ Pos(-1, -1)
  private val grid = lines.map(_.toCharArray).toArray
  private val xmas = Array('X', 'M', 'A', 'S')

  override protected def part1(): Unit = {
    var amnt = 0

    grid.findAllMatching(_ == 'X').foreach(x => amnt += directions.map(d => parse(x, 1, d)).sum)

    println(s"Part 1: $amnt")
  }

  override protected def part2(): Unit = {
    val amnt = grid.findAllMatching(_ == 'A').count(a => {
      val tlBr = grid.validPos(a + Pos(-1, -1)) && grid.validPos(a + Pos(1, 1))
        && ((grid(a.y - 1)(a.x - 1) == 'M' && grid(a.y + 1)(a.x + 1) == 'S')
        || (grid(a.y - 1)(a.x - 1) == 'S' && grid(a.y + 1)(a.x + 1) == 'M'))

      val trBl = grid.validPos(a + Pos(1, -1)) && grid.validPos(a + Pos(-1, 1))
        && ((grid(a.y - 1)(a.x + 1) == 'M' && grid(a.y + 1)(a.x - 1) == 'S')
        || (grid(a.y - 1)(a.x + 1) == 'S' && grid(a.y + 1)(a.x - 1) == 'M'))
      tlBr && trBl
    })

    println(s"Part 2: $amnt")
  }

  private def parse(point: Pos, idx: Int, direction: Pos): Int = {
    if (idx == xmas.length)
      return 1

    val nextPoint = Pos(point.x + direction.x, point.y + direction.y)
    if (grid.validPos(nextPoint) && grid.value(nextPoint) == xmas(idx)) {
      parse(nextPoint, idx + 1, direction)
    } else {
      0
    }
  }
}

object Day4 {
  def main(args: Array[String]): Unit = {
    Day4("aoc24_d4").runAll()
    /*
      Part 1: 2549
      Part 2: 2003
     */
  }
}