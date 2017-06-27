package mancala

import org.scalatest._

class MancalaSpec extends FlatSpec with Matchers {

  "A new game" should "have `bottomNext` set to `true`" in {
    val g = Game.create
    assert(g.bottomNext == true)
  }

  "A new game" should "have 4s in all cells" in {
    val g = Game.create
    val rowOf4th = List(4, 4, 4, 4, 4, 4): List[Int]
    assert(g.top == rowOf4th)
    assert(g.bottom == rowOf4th)
  }

  "A new game" should "have empty mancalas" in {
    val g = Game.create
    assert(g.topMancala == 0)
    assert(g.bottomMancala == 0)
  }

  "Any move in mancala" should "be greater than 0" in {
    Game.create.move(0) match {
      case Left(error) => {
        assert(error == "Invalid cell #. Only numbers from 1 to 6 are allowed.")
      }
      case _ => {
        fail("Game.move accepted '0' as a valid move.")
      }
    }
  }

  "Any move in mancala" should "be less than 6" in {
    Game.create.move(7) match {
      case Left(error) => {
        assert(error == "Invalid cell #. Only numbers from 1 to 6 are allowed.")
      }
      case _ => {
        fail("Game.move accepted '7' as a valid move.")
      }
    }
  }

  "Move" should "fail on an empty cell" in {
    Game.create.move(3) match {
      case Left(error) => {
         fail(s"Valid move failed with error: '$error'.")
      }
      case Right(game) => {
        game.move(3) match {
          case Left(error) => {
            assert(error == "Invalid move. Cell #3 is empty.")
          }
          case _ => {
            fail("Game.move accepted '3' as a valid move even though the cell is empty.")
          }
        }
      }
    }
  }

  "A move not ending in mancala" should "turn the board" in {
    Game.create.move(2) match {
      case Right(game) => {
        assert(!game.bottomNext)
      }
      case Left(error) => {
        fail(s"Valid move failed with error: '$error'.")
      }
    }
  }

  "The first move ending in mancala" should "keep the bottom in turn" in {
    Game.create.move(3) match {
      case Right(game) => {
        assert(game.bottomNext == true)
      }
      case Left(error) => {
        fail(s"Valid move failed with error: '$error'.")
      }
    }
  }

  "The first move ending in mancala" should "not change top row and mancala" in {
    Game.create.move(3) match {
      case Right(game) => {
        assert(game.top == List(4, 4, 4, 4, 4, 4))
        assert(game.topMancala == 0)
      }
      case Left(error) => {
        fail(s"Valid move failed with error: '$error'.")
      }
    }
  }

  "The move " should "only increment number of cells equal to number in i-th cell" in {
    Game.create.move(1) match {
      case Right(game) => {
        assert(game.bottom == List(0, 5, 5, 5, 5, 4))
      }
      case Left(error) => {
        fail(s"Valid move failed with error: '$error'.")
      }
    }
  }

  "The first move not reaching mancala" should "not change mancalas" in {
    Game.create.move(2) match {
      case Right(game) => {
        assert(game.topMancala == 0)
        assert(game.bottomMancala == 0)
      }
      case Left(error) => {
        fail(s"Valid move failed with error: '$error'.")
      }
    }
  }

  "The first move ending in mancala" should "change bottom row and mancala" in {
    Game.create.move(3) match {
      case Right(game) => {
        assert(game.bottom == List(4, 4, 0, 5, 5, 5))
        assert(game.bottomMancala == 1)
      }
      case Left(error) => {
        fail(s"Valid move failed with error: '$error'.")
      }
    }
  }

  "This random test" should "succeed" in {
    var log = "";
    val theGame = List(3, 6, 2, 1).foldLeft(Right(Game.create): Either[String, Game]) { (g, i) =>
      g match {
        case Right(game) => {
          game.move(i)
        }
        case z => z
      }
    }
    theGame match {
      case Right(game) => {
        assert(game.bottomNext == true)
      }
      case Left(error) => {
        fail(s"Valid move failed with error: '$error'.")
      }
    }
  }
}
