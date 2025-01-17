package aoc24.d19

import com.malex1337.Day

import scala.collection.mutable

class Day19(fileName: String) extends Day(fileName) {
  private val cache = mutable.Map.empty[String, Long]
  private val patterns = lines.toList.head.split(", ").toList

  override protected def part1(): Unit = {
    println(lines.drop(2)
      .map(line => solve(line, patterns))
      .count(p => p > 0))
  }

  override protected def part2(): Unit = {
    println(lines.drop(2)
      .map(line => solve(line, patterns))
      .sum)
  }

  private def solve(str: String, patterns: List[String]): Long = {
    if (!cache.contains(str)) {
      if str.isEmpty then return 1
      var res = 0L
      patterns.filter(p => str.startsWith(p)).foreach(p => {
        res += solve(str.substring(p.length), patterns)
      })
      cache(str) = res
    }
    cache(str)
  }
}

object Day19 {
  def main(args: Array[String]): Unit = {
    Day19("aoc24_d19").runAll()
  }
}