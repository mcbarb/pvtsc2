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

  trait Level0 extends SolutionChecker {
    /* terrain for level 1*/
    val level =
      """---
        |ooo
        |oS-
        |-oo
        |-oo
        |oTo
        |o-o
        |---""".stripMargin
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

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait Level2 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo------oo-
        |oSo-ooooooo-
        |ooooooooooo-
        |-ooo--oooooo
        |-----ooT----
        |------ooooo-""".stripMargin

  }

  trait LevelNoSolution extends SolutionChecker {
    /* terrain for level 1*/
    val level =
      """---
        |---
        |oS-
        |-oo
        |oTo
        |---""".stripMargin
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
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("list of neighbors") {
    new Level1 {
      val block24 = Block(Pos(2,4),Pos(2,4))
      assert(block24.neighbors == List(
        (Block(Pos(2,2),Pos(2,3)),Left),
        (Block(Pos(2,5),Pos(2,6)),Right),
        (Block(Pos(0,4),Pos(1,4)),Up),
        (Block(Pos(3,4),Pos(4,4)),Down)
      ), "neighbors of centered standing block ((2,4),(2,4))")
    }
  }

  test("list of legal neighbors") {
    new Level1 {
      val block24 = Block(Pos(2,4),Pos(2,4))
      assert(block24.legalNeighbors == List(
        (Block(Pos(2,2),Pos(2,3)),Left),
        (Block(Pos(2,5),Pos(2,6)),Right)
      ), "neighbors of centered standing block ((2,4),(2,4))")
    }
  }

  test("neighbors with history") {
    new Level1 {
      val iter0 = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up))
      assert(iter0.toSet == Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ))
    }
  }

  test("new neighbors only") {
    new Level1 {
      val iter0 = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up))
      val iter0_onlyNew = newNeighborsOnly(iter0,Set(Block(Pos(1,2),Pos(1,3)),Block(Pos(2,2),Pos(2,2))))
      assert(iter0_onlyNew.toSet == Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ))
    }
  }

  test("printing all paths") {
    new Level0 {
      println(pathsFromStart.take(100).toList)
    }
  }

	test("optimal solution for level 1") {
    new Level1 {
      print(solution)
      assert(solve(solution) == Block(goal, goal))
    }
  }


	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

  test("corner case of no solution") {
    new LevelNoSolution {
      assert(solution.length == 0)
    }
  }


  new Level2 {
    println("solution level 2")
    println(solution)
  }

}
