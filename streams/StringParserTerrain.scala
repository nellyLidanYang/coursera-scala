package streams

import common._

trait StringParserTerrain extends GameDef {
  val level: String

  def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean =
    (pos: Pos) => !(pos.row < 0 || pos.row >= levelVector.length || pos.col < 0 || pos.col >= levelVector(0).length || levelVector(pos.row)(pos.col) == '-')

  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = (for {
    x <- 0 until levelVector.length
    y <- 0 until levelVector(0).length
    if (levelVector(x)(y) == c)
  } yield Pos(x, y)).head

  private lazy val vector: Vector[Vector[Char]] =
    Vector(level.split("\n").map(str => Vector(str: _*)): _*)

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal: Pos = findChar('T', vector)

}
