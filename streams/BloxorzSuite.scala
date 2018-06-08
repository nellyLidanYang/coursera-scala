package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val level_terrain = (level.split("\n").map(_.to[Vector])).to[Vector]

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain parse and basic funs") {
    new Level1 {
      assert(terrainFunction(level_terrain)(Pos(0, 3)) === false)
      val b0 = Block(Pos(1, 1), Pos(1, 1))
      assert((Left change b0) === Block(Pos(1, -1), Pos(1, 0)))
      assert((Right change b0) === Block(Pos(1, 2), Pos(1, 3)))
      assert((Up change b0) === Block(Pos(-1, 1), Pos(0, 1)))
      assert((Down change b0) === Block(Pos(2, 1), Pos(3, 1)))
      val b1 = Block(Pos(1, 1), Pos(1, 2))
      assert((Left change b1) === Block(Pos(1, 0), Pos(1, 0)))
      assert((Right change b1) === Block(Pos(1, 3), Pos(1, 3)))
      assert((Up change b1) === Block(Pos(0, 1), Pos(0, 2)))
      assert((Down change b1) === Block(Pos(2, 1), Pos(2, 2)))
      val b2 = Block(Pos(1, 1), Pos(2, 1))
      assert((Left change b2) === Block(Pos(1, 0), Pos(2, 0)))
      assert((Right change b2) === Block(Pos(1, 2), Pos(2, 2)))
      assert((Up change b2) === Block(Pos(0, 1), Pos(0, 1)))
      assert((Down change b2) === Block(Pos(3, 1), Pos(3, 1)))
    }
  }

  test("neighborhood test"){
    new Level1 {
      val b0 = Block(Pos(1, 1), Pos(1, 1))
      assert(b0.legalNeighbors === List((Block(Pos(1, 2), Pos(1,3)), Right), (Block(Pos(2, 1), Pos(3, 1)), Down)))

      assert(neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)).toSet === Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ))

      assert(newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).toStream,

        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      ) ===   Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream)
    }
  }

	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
      assert(!terrain(Pos(4, 4)), "4, 4")
    }
  }

  test("path from start") {
    new Level1 {
      assert(pathsFromStart.take(5).toList === List(
        (Block(Pos(1,2), Pos(1,3)), List(Right)),
        (Block(Pos(2,1), Pos(3,1)), List(Down)),
        (Block(Pos(1,4), Pos(1,4)), List(Right, Right)),
        (Block(Pos(2,2), Pos(2,3)), List(Down, Right)),
        (Block(Pos(2,2), Pos(3,2)), List(Right, Down))))
    }
  }


	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

	test("optimal solution for level 1") {
    new Level1 {
      //println(pathsToGoal.take(2).toList)
      assert(solve(solution) == Block(goal, goal))
    }
  }


	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

}
