package aoc24.d10

import com.malex1337.Day
import lib.Pos

import scala.collection.mutable
import lib.GridExtensions._

class Day10(fileName: String) extends Day(fileName) {
  private val grid = lines.map(_.split("").map(_.toInt)).toArray
  private val trailHeads = grid.findAllMatching(_ == 0)

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

    if (grid.value(curr) == 9 && !visitedPeaks.contains(curr)) {
      visitedPeaks.addOne(curr)
      return score + 1
    }

    Pos.allDirections.foreach(d => {
      val next = curr + d
      if (grid.validPos(next) && !visited.contains(next) && grid.value(next) == grid.value(curr) + 1)
        score += traverse1(next, visited :+ next, visitedPeaks)
    })
    score
  }

  private def traverse2(curr: Pos, visited: List[Pos]): Int = {
    var rating = 0
    if (grid.value(curr) == 9) return rating + 1

    Pos.allDirections.foreach(d => {
      val next = curr + d
      if (grid.validPos(next) && !visited.contains(next) && grid.value(next) == grid.value(curr) + 1)
        rating += traverse2(next, visited :+ next)
    })
    rating
  }
}

object Day10 {
  def main(args: Array[String]): Unit = {
    Day10("aoc24_d10").runAll()
  }
}