package aoc24.d7

import com.malex1337.Day

class Day7(fileName: String) extends Day(fileName) {
  val list = lines.map:
    case s"$res: $numbers" => (res.toLong, numbers.split(" ").map(_.toLong).toList)

  override protected def part1(): Unit = {
    // +, *
    val equations = list.map:
      case (res, numbers) =>
        if checkEquation(res, numbers) then res else 0L
    .sum

    println(s"pt1: $equations")
  }

  private def checkEquation(res: Long, numbers: List[Long], concat: Boolean = false): Boolean = {
    numbers match
      case first :: Nil => first == res
      case first :: second :: rest =>
        checkEquation(res, (first * second) :: rest, concat) || checkEquation(res, (first + second) :: rest, concat)
          || (concat && checkEquation(res, (first.toString ++ second.toString).toLong :: rest, concat))
  }

  override protected def part2(): Unit = {
    // +, *, ||
    val equations = list.map:
      case (res, numbers) =>
        if checkEquation(res, numbers, true) then res else 0L
    .sum

    println((12.toString ++ 21.toString).toLong)

    println(s"pt2: $equations")
  }
}

object Day7 {
  def main(args: Array[String]): Unit = {
    Day7("aoc24_d7").runAll()
  }
}