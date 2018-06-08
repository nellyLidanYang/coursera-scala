package streams

import common._

/**
 * This trait represents the layout and building blocks of the game
 */
trait GameDef {
  case class Pos(row: Int, col: Int) {
    def deltaRow(d: Int): Pos = copy(row = row + d)
    def deltaCol(d: Int): Pos = copy(col = col + d)
  }

  val startPos: Pos

  val goal: Pos

  type Terrain = Pos => Boolean

  val terrain: Terrain

  sealed abstract class Move {
    def change(block: Block): Block
  }
  case object Left  extends Move {
    override def change(block: Block): Block = block.left
  }
  case object Right extends Move {
    override def change(block: Block): Block = block.right
  }
  case object Up    extends Move {
    override def change(block: Block): Block = block.up
  }
  case object Down  extends Move {
    override def change(block: Block): Block = block.down
  }

  def startBlock: Block = Block(Pos(startPos.row, startPos.col), Pos(startPos.row, startPos.col))

  val movesList = List(Left, Right, Up, Down)

  case class Block(b1: Pos, b2: Pos) {
    // checks the requirement mentioned above
    require(b1.row <= b2.row && b1.col <= b2.col, "Invalid block position: b1=" + b1 + ", b2=" + b2)

    def deltaRow(d1: Int, d2: Int) = Block(b1.deltaRow(d1), b2.deltaRow(d2))
    def deltaCol(d1: Int, d2: Int) = Block(b1.deltaCol(d1), b2.deltaCol(d2))

    def left = if (isStanding)             deltaCol(-2, -1)
               else if (b1.row == b2.row)  deltaCol(-1, -2)
               else                        deltaCol(-1, -1)
    def right = if (isStanding)            deltaCol(1, 2)
                else if (b1.row == b2.row) deltaCol(2, 1)
                else                       deltaCol(1, 1)
    def up = if (isStanding)               deltaRow(-2, -1)
             else if (b1.row == b2.row)    deltaRow(-1, -1)
             else                          deltaRow(-1, -2)
    def down = if (isStanding)             deltaRow(1, 2)
               else if (b1.row == b2.row)  deltaRow(1, 1)
               else                        deltaRow(2, 1)

    def neighbors: List[(Block, Move)] = for {
      move <- movesList
    } yield ((move change this), move)
    def legalNeighbors: List[(Block, Move)] = neighbors filter (_._1.isLegal)

    def isStanding: Boolean = ((b1.row == b2.row) && (b1.col == b2.col))
    def isLegal: Boolean = (terrain(b1) && terrain(b2))
  }
}
