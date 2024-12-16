package aoc24.d2

import com.malex1337.Day

class Day2(fileName: String) extends Day(fileName) {
  private val minDiff = 1
  private val maxDiff = 3

  override protected def part1(): Unit = {
    val amounts = lines.map(_.split(" "))
      .map(_.map(_.toInt))
      .map(isSafe)
      .count(x => x)

    println(s"Part 1: $amounts")
  }

  override protected def part2(): Unit = {
    val values = lines.map(_.split(" "))
      .map(_.map(_.toInt))

    var safes = 0
    for (value <- values) {
      if (isSafe(value)) {
        safes += 1
      } else {
        if (allArrayVariations(value).map(isSafe).exists(identity))
          safes += 1
      }
    }

    println(s"Part 2: $safes")
  }

  private def isSafe(values: Array[Int]): Boolean = {
    val ascending = values.sliding(2).forall {
      case Array(x, y) => x < y && (y - x) >= 1 && (y - x) <= 3
    }

    val descending = values.sliding(2).forall {
      case Array(x, y) => x > y && (x - y) >= 1 && (x - y) <= 3
    }
    ascending || descending
  }

  private def allArrayVariations(values: Array[Int]): Array[Array[Int]] = {
    for (i <- values.indices.toArray) yield {
      values.take(i) ++ values.drop(i + 1)
    }
  }
}

object Day2 {
  def main(args: Array[String]): Unit = {
    Day2("aoc24_d2").runAll()
  }
}