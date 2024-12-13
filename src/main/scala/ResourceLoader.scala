package com.malex1337

import scala.io.Source

object ResourceLoader {
  def loadFileFromResources(filename: String): Array[String] = {
    val source = Source.fromInputStream(getClass.getResourceAsStream(filename))
    try {
      source.getLines().toArray
    } finally {
      source.close()
    }
  }
}