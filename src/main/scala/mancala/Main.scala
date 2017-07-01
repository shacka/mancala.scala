package mancala

object MancalaMainLoop extends App { args: Array[String] =>
  override def main(args: Array[String]): Unit = {
    val moves = args.map({ x => x.toInt })
    if (moves.length > 0) processMoves(moves) else interactiveGame(Game.create)
  }

  def printResults(game: Game): Unit = {
      val player = if (game.bottomMancala > game.topMancala) "Bottom" else "Top"
      println(s"CONGRATULATIONS! $player player won!")
  }

  def printError(error: String): Unit = {
    println(s"!!! $error !!!")
  }

  def interactiveGame(game: Game): Unit = {
    println(game)
    if (game.ended) {
      printResults(game)
    } else {
      print(if (game.bottomNext) "Bottom's move: " else "Top's move: ")
      val input = readLine
      val newGame = if (! input.matches("[0-9]+")) {
        printError(s"'$input' isn't a valid input. Please only use numbers.")
        game
      } else {
        var move = input.toInt
        game.move(move) match {
          case Right(newGame) => newGame
          case Left(error) => {
            printError(error)
            game
          }
        }
      }
      interactiveGame(newGame)
    }
  }

  def processMove(game: Game, move: Int): Game = {
    println(game.toString)
    val active = if (game.bottomNext) "Bottom" else "Top"
    println(s"$active's move: $move")
    game.move(move) match {
      case Right(newGame) => {
        newGame
      }
      case Left(error) => {
        printError(error)
        game
      }
    }
  }

  def processMoves(moves: Array[Int]) = {
    val theGame = moves.foldLeft(Game.create)(processMove)
    println(theGame)
  }
}
