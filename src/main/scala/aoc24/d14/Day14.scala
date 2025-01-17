package aoc24.d14

import com.malex1337.Day

class Day14(fileName: String) extends Day(fileName) {
  val width = 101
  val height = 103

  override protected def part1(): Unit = {
    var robots = readRobots()
    for (i <- 1 to 100) {
      robots = robots.map(r => r.blink(width, height))
    }

    printState(robots)

    val quadrants = robots.map(r => r.quadrant(width, height))
      .groupBy(identity)
      .filter((k, _) => k > 0)
      .map((_, v) => v.size)
      .product

    println(quadrants)
  }

  override protected def part2(): Unit = {
    val stamSize = 20
    var blinks = 0
    var maybeFound = false
    var robots = readRobots()

    while (!maybeFound) {
      blinks += 1
      robots = robots.map(r => r.blink(width, height))

      val possibleStam = robots.groupBy(r => r.position._1).values.filter(_.size >= stamSize)

      if (possibleStam.nonEmpty) {
        val stams = possibleStam.map(l => l.sortBy(_.position._2))
          .map(l => l.sliding(2).collect {
            case List(a, b) if b.position._2 == a.position._2 + 1 => List(a, b)
          }).toList
          .find(l => l.size >= stamSize)

        maybeFound = stams.isDefined
      }
    }

    println(s"blinks to find stam with height $stamSize: $blinks")
    printState(robots)
  }

  private def readRobots(): List[Robot] = lines.map {
    case s"p=$px,$py v=$vx,$vy" => Robot((px.toInt, py.toInt), (vx.toInt, vy.toInt))
  }.toList

  private def printState(robots: List[Robot]): Unit = {
    val sb: StringBuilder = new StringBuilder()

    for (i <- 0 to height) {
      for (j <- 0 to width) {
        if (!robots.exists(r => r.position._1 == j && r.position._2 == i)) {
          sb.append(".")
        } else {
          sb.append("#")
        }
      }
      sb.append("\n")
    }

    println(sb)
  }

  case class Robot(var position: (Int, Int), velocity: (Int, Int)) {
    def blink(width: Int, height: Int): Robot = {
      Robot((
        ((position._1 + velocity._1) % width + width) % width, ((position._2 + velocity._2) % height + height) % height),
        velocity)
    }

    def quadrant(width: Int, height: Int): Int = {
      val midx = width / 2
      val midy = height / 2

      this.position match {
        case (x, y) if x < midx && y < midy => 1
        case (x, y) if x > midx && y < midy => 2
        case (x, y) if x < midx && y > midy => 3
        case (x, y) if x > midx && y > midy => 4
        case _ => -1 // on middle
      }
    }
  }
}

object Day14 {
  def main(args: Array[String]): Unit = {
    Day14("aoc24_d14").runAll()
  }
}