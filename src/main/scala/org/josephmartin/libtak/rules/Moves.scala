package org.josephmartin.libtak.rules

import org.josephmartin.libtak.core.Color.Color
import org.josephmartin.libtak.core.Direction._
import org.josephmartin.libtak.core.PieceType._
import org.josephmartin.libtak.core.{Board, Position, Reserves, Stack}
import org.josephmartin.libtak.util.ModifyWithState._

sealed trait Move {
  final val applyTo: (Board) => Board = {
    val advancePly: (Board) => Board = Board.ply.modify(_ + 1)
    val nextPlayer: (Board) => Board = Board.curPlayer.modify(_.next)

    applyToImpl andThen advancePly andThen nextPlayer
  }

  def isValidOn(board: Board): Boolean
  protected def applyToImpl: (Board) => Board
}

final case class Placement(position: Position, pieceType: PieceType) extends Move {
  private def pieceColor(board: Board): Color = {
    if (board.ply <= 2) {
      board.curPlayer.next
    } else {
      board.curPlayer
    }
  }

  override def isValidOn(board: Board): Boolean = {
    val piecesOfTypeInReserve =
      Board.reserves(pieceColor(board)) composeLens
      Reserves(pieceType) get board

    (board valid position) && piecesOfTypeInReserve > 0 && board(position).isEmpty
  }

  override protected def applyToImpl: (Board) => Board = {
    val removeReserves = (board: Board) =>
        Board.reserves(pieceColor(board)) composeLens
        Reserves(pieceType) modify { _ - 1 } apply board

    val placePiece = (board: Board) =>
      Board.stack(position) modify { _.push_top(pieceColor(board), pieceType) } apply board

    removeReserves andThen placePiece
  }
}

final case class Movement(position: Position, direction: Direction, slides: Vector[Byte]) extends Move {
  override def isValidOn(board: Board): Boolean = {
    val endPosition = position.offset(direction, slides.length)
    val interveningStacks = Board.stacksInPath(position offset direction, direction, slides.length - 1)

    def validWallFlatten =
      board(endPosition).topPiece == Wall &&
      board(position).topPiece == Cap &&
      slides.last == 1

    slides.nonEmpty &&
    slides.length < board.size &&
    slides.forall(_ > 0) &&
    board.valid(position) &&
    board.valid(endPosition) &&
    board(position).isControlledBy(board.curPlayer) &&
    board(position).height >= slides.length &&
    interveningStacks.all(_.topPiece == Flat)(board) &&
    (board(endPosition).topPiece == Flat || validWallFlatten)
  }

  override protected def applyToImpl: (Board) => Board = {
    final case class MoveState(pieces: Stack, slides: Vector[Byte])

    val pickUp = Board.stack(position).modifyS(
      MoveState(new Stack, slides),
      (state: MoveState, stack: Stack) => {
        val numPiecesMoved = state.slides.sum
        val (remaining, popped) = stack.pop_top(numPiecesMoved)
        (MoveState(state.pieces.push_top(popped), slides), remaining)
      })(_)

    val stacks = Board.stacksInPath(position.offset(direction), direction, slides.length)
    val putDown = stacks.modifyS(
      (state: MoveState, stack: Stack) => {
        val (remaining, popped) = state.pieces.pop_bottom(state.slides.head)
        (MoveState(remaining, state.slides.drop(1)), stack.push_top(popped))
      })(_)

    pickUp andThen putDown andThen { _._2 }
  }
}

