package aoc24.d12

import com.malex1337.Day
import lib.Pos

import scala.collection.mutable
import lib.GridExtensions._

class Day12(fileName: String) extends Day(fileName) {
  private val grid = lines.map(_.toCharArray).toArray
  var visited: Array[Array[Boolean]] = Array.fill(grid.length, grid(0).length)(false)

  override protected def part1(): Unit = {
    var plots: mutable.Map[Char, (Int, Int)] = mutable.Map()

    var res = 0
    for (row <- grid.indices; col <- grid(row).indices if !visited(row)(col)) {
      val char = grid(row)(col)
      val (area, perimeter) = dfs(Pos(col, row), char)
      res += area * perimeter
    }

    // area == amount of fields
    // perimeter = sides not touching each other

    println(res)
  }

  override protected def part2(): Unit = {

    val regions = findRegions()

    var costs = 0
    for (r <- regions) {
      var num_corners = 0
      for (p <- r) {
        Array(Pos(1, 1), Pos(1, -1), Pos(-1, 1), Pos(-1, -1)).foreach(d => {
          val rowNeighbor = Pos(p.col(), p.row() + d.row())
          val colNeighbor = Pos(p.col() + d.col(), p.row())
          val diagonalNeighbor = Pos(p.col() + d.col(), p.row() + d.row())

          // exterior corners
          if !r.contains(rowNeighbor) && !r.contains(colNeighbor) then num_corners += 1

          // interior corners
          if r.contains(rowNeighbor) && r.contains(colNeighbor) && !r.contains(diagonalNeighbor) then num_corners += 1
        })
      }

      costs += r.size * num_corners
    }

    println(s"costs are $costs")

  }

  private def findRegions(): mutable.Set[mutable.Set[Pos]] = {
    val visited: mutable.Set[Pos] = mutable.Set()

    val regions = mutable.Set[mutable.Set[Pos]]()
    for (row <- grid.indices; col <- grid(row).indices) {
      if (!visited.contains(Pos(col, row))) {
        val region = mutable.Set[Pos]()
        region.add(Pos(col, row))
        visited.add(Pos(col, row))
        region.addAll(findNeighbor(grid.value(Pos(col, row)), Pos(col, row), visited))
        regions.add(region)
      }
    }
    regions
  }

  private def findNeighbor(c: Char, p: Pos, visited: mutable.Set[Pos]): mutable.Set[Pos] = {
    var positions = mutable.Set[Pos]()

    Pos.allDirections.foreach(d => {
      val next = p + d
      if (grid.validPos(next) && grid(next.row())(next.col()) == c) {
        if (!visited.contains(next)) {
          visited.add(next)
          positions.add(next)
          positions = positions.addAll(findNeighbor(grid(next.row())(next.col()), next, visited))
        }
      }
    })

    positions
  }

  // DFS - Depth first search
  // First expands in every possible branch, then searches/adds up
  private def dfs(pos: Pos, c: Char): (Int, Int) = {
    visited.set(pos, true)
    var area = 1
    var perimeter = 0

    Pos.allDirections.foreach(dir => {
      val newPos = pos + dir
      if (grid.validPos(newPos) && grid.value(newPos) == c && !visited.value(newPos)) {
        val (subArea, subPerimeter) = dfs(newPos, grid.value(newPos))
        area += subArea
        perimeter += subPerimeter
      } else if (!grid.validPos(newPos) || grid.value(newPos) != c) {
        perimeter += 1
      }
    })
    (area, perimeter)
  }
}

object Day12 {
  def main(args: Array[String]): Unit = Day12("aoc24_d12").runAll()
}