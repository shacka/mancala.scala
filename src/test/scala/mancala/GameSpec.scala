package mancala

import org.scalatest._

class MancalaSpec extends FlatSpec with Matchers {

  "The new game object" should "have `next` set to 'Bottom'" in {
    val g = Game.create()
    assert(g.next == Bottom)
  }

  "The new game object" should "have 4th in all cells" in {
    val g = Game.create()
    val rowOf4th = List(4, 4, 4, 4, 4, 4): List[Int]
    assert(g.top == rowOf4th)
    assert(g.bottom == rowOf4th)
  }

  "The new game object" should "have empty mancalas" in {
    val g = Game.create()
    assert(g.topMancala == 0)
    assert(g.bottomMancala == 0)
  }

  "Any move in mancala" should "be greater than 0" in {
    Game.move(Game.create(), 0) match {
      case Left((error, _)) => {
        assert(error == "Invalid move. Only numbers from 1 to 6 are allowed.")
      }
      case _ => {
        fail("Game.move accepted '0' as a valid move.")
      }
    }
  }

  "Any move in mancala" should "be less than 6" in {
    Game.move(Game.create(), 7) match {
      case Left((error, _)) => {
         assert(error == "Invalid move. Only numbers from 1 to 6 are allowed.")
      }
      case _ => {
        fail("Game.move accepted '7' as a valid move.")
      }
    }
  }

  "Move" should "fail on an empty cell" in {
    Game.move(Game.create(), 3) match {
      case Left((error, _)) => {
         fail(s"Valid move failed with error: '$error'.")
      }
      case Right(aGame) => {
        Game.move(aGame, 3) match {
          case Left((error, _)) => {
            assert(error == "Invalid move. Cell #3 is empty.")
          }
          case _ => {
            fail("Game.move accepted '3' as a valid move even though the cell is empty.")
          }
        }
      }
    }
  }

  "A move not ending in mancala" should "switch next" in {
    Game.move(Game.create(), 2) match {
      case Right(game) => {
        assert(game.next == Top)
      }
      case Left((error, _)) => {
        fail(error)
      }
    }
  }

  "The first move ending in mancala" should "keep the bottom in turn" in {
    Game.move(Game.create(), 3) match {
      case Right(game) => {
        assert(game.next == Bottom)
      }
      case Left((error, _)) => {
        fail(error)
      }
    }
  }

  "The first move ending in mancala" should "not change top row and mancala" in {
    Game.move(Game.create(), 3) match {
      case Right(game) => {
        assert(game.top == List(4, 4, 4, 4, 4, 4))
        assert(game.topMancala == 0)
      }
      case Left((error, _)) => {
        fail(error)
      }
    }
  }

  "The move " should "only increment number of cells equal to number in i-th cell" in {
    Game.move(Game.create(), 1) match {
      case Right(game) => {
        assert(game.bottom == List(0, 5, 5, 5, 5, 4))
      }
      case Left((error, game)) => {
        fail(error ++ game.toString)
      }
    }
  }

  "The first move not reaching mancala" should "not change mancalas" in {
    Game.move(Game.create(), 2) match {
      case Right(game) => {
        assert(game.topMancala == 0)
        assert(game.bottomMancala == 0)
      }
      case Left((error, game)) => {
        fail(error ++ game.toString)
      }
    }
  }

  "The first move ending in mancala" should "change bottom row and mancala" in {
    Game.move(Game.create(), 3) match {
      case Right(game) => {
        assert(game.bottom == List(4, 4, 0, 5, 5, 5))
        assert(game.bottomMancala == 1)
      }
      case Left((error, _)) => {
        fail(error)
      }
    }
  }
}
