/**
  * Created by josephm on 8/5/17.
  */
import org.josephmartin.libtak.core.{Board, Position}
import org.josephmartin.libtak.core.Direction._
import org.josephmartin.libtak.core.PieceType._
import org.josephmartin.libtak.rules.{Movement, Placement}

object test {
  def main(args: Array[String]): Unit = {
    val b = new Board(5)
      .execute(Placement(Position(0,0), Wall))
      .execute(Placement(Position(0,0), Wall))
      .execute(Placement(Position(0,0), Wall))
      .execute(Placement(Position(0,0), Wall))
      .execute(Placement(Position(0,0), Wall))
      .execute(Placement(Position(0,0), Wall))
      .execute(Placement(Position(0,0), Cap))
    val newB = b.execute(Movement(Position(0,0), North, Vector(1, 2, 4)))

    Board.stacks.getAll(b).foreach(println(_))
    println("---")
    Board.stacks.getAll(newB).foreach(println(_))
  }
}