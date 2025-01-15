package aoc24.d11

import com.malex1337.Day

class Day11(fileName: String) extends Day(fileName) {

  override protected def part1(): Unit = {
    var stones = lines.flatMap(l => l.split(" ")).map(i => i.toLong)
    println(stones)

    for (i <- 1 to 25) {
      stones = blink(stones)
    }
    println(stones.length)
  }

  override protected def part2(): Unit = {
    var stonesFreq = lines
      .flatMap(l => l.split(" ").map(i => i.toLong))
      .groupBy(identity)
      .map((k, v) => (k, v.size.toLong))

    for(i <- 1 to 75)
      stonesFreq = blink2(stonesFreq)

    println(stonesFreq.values.sum)
  }

  private def blink(list: Seq[Long]): Seq[Long] =
    list.flatMap:
      case 0 => 1 :: Nil
      case EvenDigits(a, b) => a :: b :: Nil
      case other => other * 2024 :: Nil

  private def blink2(stones: Map[Long, Long]): Map[Long, Long] =
    stones.foldLeft(stones): (newList, stone) =>
      stone match
        case (0, n) => newList.adaptFrequency(0, -n).adaptFrequency(1, n)
        case (old@EvenDigits(a, b), n) => newList.adaptFrequency(old, -n).adaptFrequency(a, n).adaptFrequency(b, n)
        case (other, n) => newList.adaptFrequency(other, -n).adaptFrequency(other * 2024, n)

  extension (stones: Map[Long, Long])
    private def adaptFrequency(stone: Long, change: Long): Map[Long, Long] =
      stones.updatedWith(stone):
        case None => Some(change)
        case Some(n) =>
          val n0 = n + change
          if n0 == 0 then None else Some(n0)
}

object EvenDigits:
  def unapply(n: Long): Option[(Long, Long)] = splitDigits(n)

  private def splitDigits(l: Long): Option[(Long, Long)] = {
    val str = l.toString

    if str.length % 2 != 0 then return None

    val middle = str.length / 2

    val (first, second) = str.splitAt(middle)
    Some(first.toInt, second.toInt)
  }

object Day11 {
  def main(args: Array[String]): Unit = {
    Day11("aoc24_d11").runAll()
  }
}