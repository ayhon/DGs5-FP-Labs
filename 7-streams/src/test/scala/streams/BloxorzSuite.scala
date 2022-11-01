package streams

import Bloxorz.*

class BloxorzSuite extends munit.FunSuite:
  trait SolutionChecker extends GameDef with Solver with StringParserTerrain:
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    import Move.*
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
    }

  trait Level1 extends SolutionChecker:
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    import Move.*
    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)

  trait CustomLevel extends SolutionChecker

  test("terrain function level 1 (10pts)") {
    new Level1:
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

  test("find char level 1 (10pts)") {
    new Level1:
      assertEquals(startPos, Pos(1, 1))
  }


  test("optimal solution for level 1 (5pts)") {
    new Level1:
      assertEquals(solve(solution), Block(goal, goal))
  }


  test("optimal solution length for level 1 (5pts)") {
    new Level1:
      assertEquals(solution.length, optsolution.length)
  }
  
  test("neighboursWithHistory works on given example"){
    new Level1: 
      import Move.*
      val tested = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)).toSet
      val sol = Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        )
      assertEquals(tested, sol)
  }
  
  test("newNeighborsOnly works on given example"){
    new Level1:
      import Move.*
      val tested = newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).to(LazyList),

        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      )
      val sol = Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).to(LazyList)
      assertEquals(tested, sol)
  }
  
  test("from works as expected"){
    new CustomLevel:
      import Move.*
      val level =
      """ooo-------
        |oTooooooo-
        |ooooooooo-
        |-ooooooooo
        |-oooooooSo
        |------ooo-""".stripMargin
      val optsolution = List(Up, Left, Up, Left, Left, Left, Left)

      val initial = LazyList(
        ( Block(Pos(4,8),Pos(4,8)), Nil),        // ###1##
        ( Block(Pos(4,6),Pos(4,7)), List(Left)), // ###1##
        ( Block(Pos(2,8),Pos(3,8)), List(Up)),   // #220##
      )
      val explored = Set(
        Block(Pos(4,8),Pos(4,8)),  
        Block(Pos(4,6),Pos(4,7)),
        Block(Pos(2,8),Pos(3,8)),
      )
      val res = from(initial,explored)
      assertEquals(
        res.take(5).toSet,
        Set(
          ( Block(Pos(4,5),Pos(4,5)), List(Left,Left)),
          ( Block(Pos(5,6),Pos(5,7)), List(Down,Left)),
          ( Block(Pos(3,6),Pos(3,7)), List(Up,Left)),
          ( Block(Pos(2,7),Pos(3,7)), List(Left,Up)),
          ( Block(Pos(1,8),Pos(1,8)), List(Up,Up)),
        )
      )
      assertEquals(
        solution,
        optsolution
      )
  }
  
  test("Testing"){
    // new Level1:
    //   println(solution)
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds

/*

pathsFromStart
(startBlock, Nil) #:: from(pathsFromStart, Set())
(startBloc, Nil) #:: ↓
   case (startBlock, Nil) :: from(pathsFromStart, Set()) =>
     LazyList[(Block, List[Move])] ++ from(pathsFromStart, Set(startBlock))




*/