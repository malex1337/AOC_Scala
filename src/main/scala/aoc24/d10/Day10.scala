package aoc24.d10

import com.malex1337.Day
import lib.Pos

import scala.collection.mutable

class Day10(fileName: String) extends Day(fileName) {
  private val trailhead = 0

  private val grid = lines.map(_.split("").map(_.toInt)).toArray
  private val rows = grid.length
  private val cols = grid(0).length
  private val trailHeads = for
    row <- grid.indices
    col <- grid(row).indices
    if grid(row)(col) == trailhead
  yield Pos(col, row)

  override protected def part1(): Unit = {
    val scores = trailHeads.map(pos => traverse1(pos, List(pos), mutable.ListBuffer(pos))).sum
    println(scores)
  }

  override protected def part2(): Unit = {
    val scores = trailHeads.map(pos => traverse2(pos, List(pos))).sum
    println(scores)
  }

  private def traverse1(curr: Pos, visited: List[Pos], visitedPeaks: mutable.ListBuffer[Pos]): Int = {
    var score = 0

    if (gridValue(curr) == 9 && !visitedPeaks.contains(curr)) {
      visitedPeaks.addOne(curr)
      return score + 1
    }

    Pos.allDirections.foreach(d => {
      val next = curr + d
      if (inGrid(next) && !visited.contains(next) && gridValue(next) == gridValue(curr) + 1)
        score += traverse1(next, visited :+ next, visitedPeaks)
    })
    score
  }

  private def traverse2(curr: Pos, visited: List[Pos]): Int = {
    var rating = 0
    if (gridValue(curr) == 9) return rating + 1

    Pos.allDirections.foreach(d => {
      val next = curr + d
      if (inGrid(next) && !visited.contains(next) && gridValue(next) == gridValue(curr) + 1)
        rating += traverse2(next, visited :+ next)
    })
    rating
  }

  private def gridValue(pos: Pos) = grid(pos.row())(pos.col())

  private def inGrid(pos: Pos) = pos.row() >= 0 && pos.col() >= 0 && pos.row() < grid.length && pos.col() < grid(0).length
}

object Day10 {
  def main(args: Array[String]): Unit = {
    Day10("aoc24_d10").runAll()
  }
}