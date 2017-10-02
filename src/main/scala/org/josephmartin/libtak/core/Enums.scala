package org.josephmartin.libtak.core

object Direction extends Enumeration {
  type Direction = Value
  val North, East, South, West = Value

  implicit class DirectionExt(val direction: Direction) extends AnyVal {
    def toStrideFor(board: Board) = {
      @inline val offset = Position(0,0).offset(direction)
      offset.x + offset.y * board.size
    }
  }
}

import Direction._

final case class Position(x: Int, y: Int) {
  def toIndexFor(g: Board): Int = {
    x + y * g.size
  }

  @inline def offset(direction: Direction, distance: Int = 1): Position = {
    direction match {
      case North => Position(x, y + distance)
      case East  => Position(x + distance, y)
      case South => Position(x, y - distance)
      case West  => Position(x - distance, y)
    }
  }
}

