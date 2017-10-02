package org.josephmartin.libtak.core

import Color._

sealed trait GameResult {
  def getWinner(): Option[Color]
}

final case class RoadWin(winner: Color) extends GameResult {
  override def getWinner(): Option[Color] = Some(winner)
}

final case class FlatWin(winner: Color, margin: Option[Int]) extends GameResult {
  override def getWinner(): Option[Color] = Some(winner)
}

final case class OtherWin(winner: Color) extends GameResult {
  override def getWinner(): Option[Color] = Some(winner)
}

final case class Tie() extends GameResult {
  override def getWinner(): Option[Color] = None
}


