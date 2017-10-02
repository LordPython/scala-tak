package org.josephmartin.libtak.core

import monocle.Lens
import monocle.macros.GenLens

object Color extends Enumeration {
  type Color = Value
  val White = Value(0)
  val Black = Value(1)

  @inline implicit final class ColorExt(val c: Color) extends AnyVal {
    @inline def next(): Color = {
      if (c == White) {
        Black
      } else {
        White
      }
    }
  }
}

object PieceType extends Enumeration {
  type PieceType = Value
  val Flat, Wall, Cap = Value
}

final case class Reserves private(flats: Int, caps: Int) {
  @inline def empty: Boolean = flats == 0 && caps == 0
}

object Reserves {
  import PieceType._

  def forSize(size: Int): Reserves = {
    size match {
      case 3 => new Reserves(10, 0)
      case 4 => new Reserves(14, 0)
      case 5 => new Reserves(21, 1)
      case 6 => new Reserves(30, 1)
      case 7 => new Reserves(40, 2)
      case 8 => new Reserves(50, 2)
    }
  }

  def apply(p: PieceType): Lens[Reserves, Int] = {
    p match {
      case Flat => GenLens[Reserves](_.flats)
      case Wall => GenLens[Reserves](_.flats)
      case Cap => GenLens[Reserves](_.caps)
    }
  }
}

