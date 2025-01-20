package aoc24.d15

import com.malex1337.Day
import lib.Pos

import scala.collection.mutable.ListBuffer
import scala.util.boundary
import scala.util.boundary.break
import scala.util.control.Breaks

class Day15(fileName: String) extends Day(fileName) {
  private val dirOffset = Map(
    '^' -> Pos.up,
    '>' -> Pos.right,
    'v' -> Pos.down,
    '<' -> Pos.left,
  )

  private val reverseOffset = Map(
    Pos.up -> '^',
    Pos.right -> '>',
    Pos.down -> 'v',
    Pos.left -> '<'
  )

  private val expansion = Map(
    '#' -> "##",
    'O' -> "[]",
    '.' -> "..",
    '@' -> "@."
  )

  private val grid = lines.takeWhile(_.nonEmpty).map(_.toCharArray).map(c => c.map(Cell.fromChar(_))).toArray
  private val moves = lines.drop(grid.length).flatMap(_.toCharArray).map(c => dirOffset(c)).toArray
  private var robot = findRobot()

  override protected def part1(): Unit = {
    printMoves()
    println("\n")
    printGrid()

    moves.foreach(move => {
      var boxes: List[Pos] = List()
      var shouldMove = true

      var current = robot
      boundary {
        while (true) {
          current += move
          grid(current.y)(current.x) match {
            case Cell.Border =>
              shouldMove = false
              break()
            case Cell.Box =>
              boxes = boxes :+ current
            case Cell.Free => break()
            case Cell.Robot => break() // should never happen
          }
        }
      }

      if (shouldMove) {
        grid(robot.y)(robot.x) = Cell.Free
        grid(robot.y + move.y)(robot.x + move.x) = Cell.Robot
        boxes.foreach(b => grid(b.y + move.y)(b.x + move.x) = Cell.Box)
        robot = robot + move
      }
    })

    var gpsSum = 0

    for (i <- grid.indices; j <- grid(i).indices) {
      if grid(i)(j) == Cell.Box then gpsSum += (100 * i) + j
    }

    println(gpsSum)
  }

  override protected def part2(): Unit = {
    val expandedGrid: Array[Array[Char]] = lines.takeWhile(_.nonEmpty).map(_.toCharArray).map(c => c.flatMap(c2 => expansion(c2))).toArray
    robot = findRobotExpanded(expandedGrid)

    moves.foreach(move => {
      var boxes: ListBuffer[Pos] = ListBuffer(robot)
      var shouldMove = true

      val outerLoop = new Breaks
      outerLoop.breakable {
        var i = 0
        while (i < boxes.length) {
          val box = boxes(i)
          val innerLoop = new Breaks
          innerLoop.breakable {
            val nextPos = box + move
            if boxes.contains(nextPos) then innerLoop.break()
            expandedGrid(nextPos.y)(nextPos.x) match {
              case '#' =>
                shouldMove = false
                outerLoop.break()
              case '[' =>
                boxes += nextPos
                boxes += (nextPos + Pos.right)
              case ']' =>
                boxes += (nextPos + Pos.left)
                boxes += nextPos
              case _ =>
            }
          }
          i += 1
        }
      }
      if (shouldMove) {
        val gridCopy = expandedGrid.map(_.clone())
        expandedGrid(robot.y)(robot.x) = '.'
        expandedGrid(robot.y + move.y)(robot.x + move.x) = '@'

        boxes.drop(1).foreach(box => {
          expandedGrid(box.y)(box.x) = '.'
        })

        boxes.drop(1).foreach(box => {
          expandedGrid(box.y + move.y)(box.x + move.x) = gridCopy(box.y)(box.x)
        })

        robot = robot + move
      }

      //printExpandedGrid(expandedGrid)
    })

    var gpsSum = 0
    for (i <- expandedGrid.indices; j <- expandedGrid(i).indices) {
      if expandedGrid(i)(j) == '[' then gpsSum += (100 * i) + j
    }
    println(gpsSum)
  }

  private def printGrid(): Unit =
    for (j <- grid.indices) {
      for (i <- grid(j).indices) {
        print(grid(j)(i) match {
          case Cell.Border => '#'
          case Cell.Free => '.'
          case Cell.Robot => '@'
          case Cell.Box => 'O'
        })
        print(" ")
      }
      println()
    }

  private def printExpandedGrid(expandedGrid: Array[Array[Char]]): Unit =
    for (i <- expandedGrid.indices) {
      for (j <- expandedGrid(i).indices) {
        print(expandedGrid(i)(j))
      }
      println()
    }

  private def printMoves(): Unit = moves.foreach(d => print(reverseOffset(d) + " "))

  private def findRobot(): Pos = boundary {
    for (i <- grid.indices; j <- grid(i).indices) {
      if (grid(i)(j) == Cell.Robot) break(Pos(j, i))
    }
    Pos(-1, -1)
  }

  private def findRobotExpanded(expandedGrid: Array[Array[Char]]): Pos = boundary {
    for (i <- expandedGrid.indices; j <- expandedGrid(i).indices) {
      if (expandedGrid(i)(j) == '@') break(Pos(j, i))
    }
    Pos(-1, -1)
  }

  enum Cell:
    case Border, Box, Free, Robot

  object Cell:
    val mappings = Map(
      '#' -> Border,
      'O' -> Box,
      '@' -> Robot,
      '.' -> Free)

    def fromChar(ch: Char): Cell = mappings(ch)
}

object Day15 {
  def main(args: Array[String]): Unit = {
    Day15("aoc24_d15").runAll()
  }
}