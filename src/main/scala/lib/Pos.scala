package lib

case class Pos(x: Int, y: Int) {
  def +(that: Pos): Pos = Pos(x + that.x, y + that.y)
}

object Pos {
  val up: Pos = Pos(0, -1)
  val right: Pos = Pos(1, 0)
  val down: Pos = Pos(0, 1)
  val left: Pos = Pos(-1, 0)
}