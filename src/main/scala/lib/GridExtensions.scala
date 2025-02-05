package lib

import java.util.function.Predicate
import scala.util.control.Breaks.{break, breakable}


object GridExtensions {
  implicit class ExtendedGrid[T](grid: Array[Array[T]]) {
    def validPos(pos: Pos): Boolean = pos.row() >= 0 && pos.col() >= 0
      && pos.row() < grid.length && pos.col() < grid(0).length

    def findAllMatching(predicate: Predicate[T]): List[Pos] = {
      (for
        row <- grid.indices
        col <- grid(0).indices
        if predicate.test(grid.value(Pos(col, row)))
      yield Pos(col, row)
        ).toList
    }

    def findFirst(predicate: Predicate[T]): Option[Pos] = {
      var result: Option[Pos] = None
      breakable {
        for (row <- grid.indices) {
          for (col <- grid(row).indices) {
            if (predicate.test(grid.value(Pos(col, row)))) {
              result = Some(Pos(col, row))
              break()
            }
          }
        }
      }
      result
    }

    def set(pos: Pos, value: T): Unit = grid(pos.row())(pos.col()) = value

    def value(pos: Pos): T = grid(pos.row())(pos.col())

    def rows(): Int = grid.length

    def cols(): Int = grid(0).length

    def printGrid(): Unit =
      grid.foreach(r => {
        r.foreach(c => print(c.toString + " "))
        println
      })

    def copy(): Array[Array[T]] = grid.transpose.transpose
  }
}