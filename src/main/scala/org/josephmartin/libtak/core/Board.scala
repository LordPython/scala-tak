package org.josephmartin.libtak.core
import scala.language.higherKinds
import Color._
import PieceType._
import Direction._
import monocle.{Lens, PTraversal, Traversal}
import org.josephmartin.libtak.rules.Move
import scalaz.std.vector._

import scalaz.Applicative

final class Board private(
  val size: Int,
  private val board: Vector[Stack],
  val white: Reserves,
  val black: Reserves,
  val curPlayer: Color,
  val ply: Int)
{
  def this(size: Int) {
    this(size, Vector.fill(size*size)(new Stack()), Reserves.forSize(size), Reserves.forSize(size), White, 1)
  }

  def valid(position: Position): Boolean = {
    position.x < size && position.y < size
  }

  def countFlats(player: Color): Int = {
    board.count(stack => stack.height > 0 && stack(0) == player && stack.topPiece == Flat)
  }

  def checkRoadWin(player: Color): Option[GameResult] = {
    None
  }

  def checkFlatWin(): Option[GameResult] = {
    if (Board.reserves.exist(_.empty)(this) || Board.stacks.all(_.height > 0)(this)) {
      val whiteFlats = countFlats(White)
      val blackFlats = countFlats(Black)

      Some(if (whiteFlats > blackFlats) {
        FlatWin(White, Some(whiteFlats - blackFlats))
      } else if (blackFlats > whiteFlats) {
        FlatWin(Black, Some(blackFlats - whiteFlats))
      } else {
        Tie()
      })
    } else {
      None
    }
  }

  def execute(move: Move): Board = {
    move applyTo this
  }

  def status(): Option[GameResult] = None
    .orElse(checkRoadWin(curPlayer.next()))
    .orElse(checkRoadWin(curPlayer))
    .orElse(checkFlatWin())

  def apply(position: Position): Stack = {
    board(position toIndexFor this)
  }

  private def copy(
    size: Int = size,
    board: Vector[Stack] = board,
    white: Reserves = white,
    black: Reserves = black,
    curPlayer: Color = curPlayer,
    ply: Int = ply):
  Board = {
    new Board(size, board, white, black, curPlayer, ply)
  }
}

object Board {
  val white: Lens[Board, Reserves] = Lens[Board, Reserves](_.white)(w => g => g.copy(white = w))
  val black: Lens[Board, Reserves] = Lens[Board, Reserves](_.black)(b => g => g.copy(black = b))
  val ply: Lens[Board, Int] = Lens[Board, Int](_.ply)(p => g => g.copy(ply = p))
  val curPlayer: Lens[Board, Color] = Lens[Board, Color](_.curPlayer)(c => g => g.copy(curPlayer = c))
  private val board = Lens[Board, Vector[Stack]](_.board)(board => g => g.copy(board = board))
  val stacks: Traversal[Board, Stack] = board composeTraversal Traversal.fromTraverse[Vector, Stack]

  def reserves(color: Color): Lens[Board, Reserves] = {
    color match {
      case White => white
      case Black => black
    }
  }

  val reserves: Traversal[Board, Reserves] = {
    Traversal
      .apply2((board: Board) => board.white, (board: Board) => board.black)((white: Reserves, black: Reserves, board: Board) => board.copy(white=white, black=black))
  }

  def stacksInPath(start: Position, direction: Direction, distance: Int): Traversal[Board, Stack] = {
    new PTraversal[Board, Board, Stack, Stack] {
      override def modifyF[F[_]](f: (Stack) => F[Stack])(game: Board)(implicit F: Applicative[F]): F[Board] = {
        val startIndex = start toIndexFor game
        val stride = direction toStrideFor game
        val indexes = 0 until distance map { startIndex + _ * stride }

        indexes.foldRight(F.point(game)) {
          (index: Int, fGame: F[Board]) => F.apply2(f(game.board(index)), fGame) {
            (stack: Stack, game: Board) => game.copy(board = game.board.updated(index, stack))
          }
        }
      }
    }
  }

  def stack(position: Position): monocle.Lens[Board, Stack] = {
    val get = (game: Board) => {
      game.board(position toIndexFor game)
    }
    val set = (s: Stack) => (game: Board) => {
      game.copy(board = game.board.updated(position toIndexFor game, s))
    }
    monocle.Lens[Board, Stack](get)(set)
  }
}

