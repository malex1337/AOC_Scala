package aoc24.d5

import com.malex1337.Day

class Day5(fileName: String) extends Day(fileName) {
  private val rules = lines.takeWhile(_.nonEmpty).collect { case s"$a|$b" => (a.toInt, b.toInt) }.toSet
  private val updates = lines.drop(rules.size + 1).map(_.split(",").map(_.toInt))

  val (good, bad) = updates.partition(update => update.zip(update.tail).forall(rules))

  override protected def part1(): Unit = {
    val (matching, _) = updates.partition(update => update.zip(update.tail).forall(rules))
    println(s"Part 1: ${sumMiddles(matching)}")
  }

  override protected def part2(): Unit = {
    val (_, nonMatching) = updates.partition(update => update.zip(update.tail).forall(rules))
    val fixedList = nonMatching.map(_.sortWith((a, b) => rules((a, b))))
    println(s"Part 2: ${sumMiddles(fixedList)}")
  }

  private def sumMiddles(updates: Seq[Array[Int]]): Int = {
    updates.map(update => update(update.length / 2)).sum
  }
}

object Day5 {
  def main(args: Array[String]): Unit = {
    Day5("aoc24_d5").runAll()
  }
}