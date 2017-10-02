package org.josephmartin.libtak.core

import scala.language.higherKinds
import Color._
import PieceType._
import monocle.{PTraversal, Traversal}
import scalaz.Applicative

class Stack private(private val owners: Long, val height: Int, val topPiece: PieceType)
  extends IndexedSeq[Color]
{
  // Default constructor: Construct an empty stack
  def this() {
    this(0, 0, Flat)
  }

  def pop_top(n: Int): (Stack, Stack) = {
    val remaining = new Stack(owners >>> n, height - n, Flat)
    val popped = new Stack(owners & ~(-1 << n), n, topPiece)
    (remaining, popped)
  }

  def pop_bottom(n: Int): (Stack, Stack) = {
    val remaining = new Stack(owners & ~(-1 << (height-n)), height-n, topPiece)
    val popped = new Stack(owners >>> (height-n), n, Flat)
    (remaining, popped)
  }

  def push_top(other: Stack): Stack = {
    new Stack(
      (this.owners << other.height) | other.owners,
      this.height + other.height,
      other.topPiece
    )
  }

  def push_top(owner: Color, pieceType: PieceType): Stack = {
    new Stack((owners << 1) | owner.id, height + 1, pieceType)
  }

  override def apply(index: Int): Color = {
    assert(isDefinedAt(index), "Index out of range: "+index)
    Color(((owners >> index) & 1).toInt)
  }

  override def length: Int = height

  def isControlledBy(color: Color): Boolean = height > 0 && this(0) == color
  def controller: Option[Color] = {
    if (height <= 0) None
    else Some(this(0))
  }
}

object Stack {
  val owners: Traversal[Stack, Color] = new PTraversal[Stack, Stack, Color, Color] {
    override def modifyF[F[_]](f: (Color) => F[Color])(s: Stack)(implicit F: Applicative[F]): F[Stack] = {
      s.foldRight(F.point(new Stack(0, 0, s.topPiece))) {
        (color, fStack) => F.apply2(f(color), fStack) {
          (color, stack) => stack.push_top(color, s.topPiece)
        }
      }
    }
  }
}
