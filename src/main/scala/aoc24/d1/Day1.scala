package aoc24.d1

import com.malex1337.Day

class Day1(fileName: String) extends Day(fileName) {

  override def part1(): Unit = {
    val (firstList, secondList) = extractTuples()

    val left = firstList.map((value, _) => value).sorted
    val right = secondList.map((value, _) => value).sorted

    val diffs = (left zip right).map((a, b) => if (a > b) a - b else b - a).sum
    println(s"Part 1: $diffs")
  }

  override def part2(): Unit = {
    val (firstList, secondList) = extractTuples()

    val left = firstList.map((value, _) => value)
    val right = secondList.map((value, _) => value)

    val sum = left.map(elem => elem -> right.count(_ == elem))
      .toMap
      .map((k, v) => k * v)
      .sum
    println(s"Part 2: $sum")
  }

  private def extractTuples(): (Array[(Int, Int)], Array[(Int, Int)]) = {
    lines
      .flatMap(line => line.split(";"))
      .map(_.toInt)
      .zipWithIndex
      .partition((_, index) => index % 2 == 0)
  }
}

object Day1 {
  def main(args: Array[String]): Unit = {
    Day1("/aoc24_d1").runAll()
  }
}