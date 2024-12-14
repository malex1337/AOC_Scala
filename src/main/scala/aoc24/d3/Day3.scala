package aoc24.d3

import com.malex1337.Day

class Day3(fileName: String) extends Day(fileName) {
  private val pattern = """mul\((\d+),(\d+)\)""".r

  override protected def part1(): Unit = {
    val sum = lines.map(line => pattern.findAllMatchIn(line).map(m => m.group(1).toInt * m.group(2).toInt).sum).sum
    println(s"Part 1: $sum")
  }

  override protected def part2(): Unit = {
    val pattern2 = """(do\(\))|(don't\(\))|(mul\((\d+),(\d+)\))""".r

    val matches = lines.flatMap(m => pattern2.findAllMatchIn(m))
    var enabled = true
    var sum = 0

    matches.foreach(m => {
      if (m.group(1) != null) {
        enabled = true
      }
      if (m.group(2) != null) {
        enabled = false
      }
      if (m.group(3) != null && enabled) {
        sum += m.group(4).toInt * m.group(5).toInt
      }
    })

    println(s"Part 2: $sum")
  }
}

object Day3 {
  def main(args: Array[String]): Unit = {
    Day3("/aoc24_d3").runAll()
  }
}