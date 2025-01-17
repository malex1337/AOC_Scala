package aoc24.d13

import com.malex1337.Day

class Day13(fileName: String) extends Day(fileName) {
  private val costA = 3

  private val games = lines.filter(_.nonEmpty).map {
      case s"Button A: X+$x, Y+$y" => (x.toLong, y.toLong)
      case s"Button B: X+$x, Y+$y" => (x.toLong, y.toLong)
      case s"Prize: X=$x, Y=$y" => (x.toLong, y.toLong)
    }.toList
    .grouped(3).collect {
      case List(a, b, p) => Game(a._1, a._2, b._1, b._2, p._1, p._2)
    }.toList

  override protected def part1(): Unit = {
    // ax s + bx t = px
    // ay s + by t = py
    // -> s = (px by - py bx) / (ax by - ay bx)   care div 0!
    // -> t = (px - ax s) / bx

    var total = 0L

    games.foreach {
      case Game(ax, ay, bx, by, px, py)
      =>
        (px * by - py * bx).safeDiv(ax * by - ay * bx) match {
          case Some(s) =>
            (px - ax * s).safeDiv(bx) match {
              case Some(t) =>
                if s <= 100 && t <= 100 then total += s * costA + t
              case _ =>
            }
          case _ =>
        }
    }

    println(total)
  }

  override protected def part2(): Unit = {
    // -> s = (px by - py bx) / (ax by - ay bx)   care div 0!
    // -> t = (px - ax s) / bx
    var total = 0L
    games.foreach {
      case Game(ax, ay, bx, by, px, py)
      =>
        val pxNew = 10000000000000L + px
        val pyNew = 10000000000000L + py
        (pxNew * by - pyNew * bx).safeDiv(ax * by - ay * bx) match {
          case Some(s) =>
            (pxNew - ax * s).safeDiv(bx) match {
              case Some(t) => total += s * costA + t
              case _ =>
            }
          case _ =>
        }
    }
    println(total)
  }

  extension (a: Long)
    private infix def safeDiv(b: Long): Option[Long] =
      Option.when(b != 0 && a % b == 0)(a / b)

  private case class Game(ax: Long, ay: Long, bx: Long, by: Long, px: Long, py: Long) {
    def unapply(arg: Game): Option[(Long, Long, Long, Long, Long, Long)] = {
      Some((arg.ax, arg.ay, arg.bx, arg.by, arg.px, arg.py))
    }
  }
}

object Day13 {
  def main(args: Array[String]): Unit = {
    Day13("aoc24_d13").runAll()
  }
}