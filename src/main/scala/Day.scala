package com.malex1337

import scala.io.Source

abstract class Day(filename: String) {
  val lines: Seq[String] = Source.fromResource(s"$filename").getLines().toSeq

  protected def part1(): Unit

  protected def part2(): Unit

  def runAll(): Unit = {
    var duration = measureTime {
      part1()
    }
    println(s"Part 1 took ${duration}ms")
    println

    duration = measureTime {
      part2()
    }
    println(s"Part 2 took ${duration}ms")
    println
  }

  private def measureTime(code: => Unit): Double = {
    val startTime = System.nanoTime()
    code
    val endTime = System.nanoTime()
    (endTime - startTime) / 1e6
  }
}