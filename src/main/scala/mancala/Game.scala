package mancala


class Game private ( val top: List[Int]
                   , val topMancala: Int
                   , val bottom: List[Int]
                   , val bottomMancala: Int
                   , val next: Side
                   ) {

  override def toString: String = {
    val topRow = top.mkString(" ")
    val bottomRow = bottom.reverse.mkString(" ")
    s"L: $topRow LM: $topMancala\nR: $bottomRow RM: $bottomMancala\nNM: $next"
  }

}


sealed abstract class Side() {
  def oposite: Side {}
}
case object Top extends Side() {
  override def toString: String = { "Top" }
  override def oposite: Side = { Bottom }
}
case object Bottom extends Side() {
  override def toString: String = { "Bottom" }
  override def oposite: Side = { Top }
}


object Game {
  def create() = {
    new Game(List.fill(6){4}, 0, List.fill(6){4}, 0, Bottom)
  }

  def move(theGame: Game, cell: Int) : Either[(String, Game), Game] = {
    val top = theGame.top
    val topM = theGame.topMancala
    val bottom = theGame.bottom
    val bottomM = theGame.bottomMancala
    getStones(theGame, cell) match {
      case Left(error) => Left(error, theGame)
      case Right(stones) => {
        val newNext = if (endsInMancala(theGame, cell, stones)) { theGame.next } else { theGame.next.oposite }
        theGame.next match {
          case Top => {
            val newTop = updateActiveRow(theGame.top, cell, stones)
            val newTopM = updateMancala(theGame.topMancala, cell, stones)
            val newBottom = updateInactiveRow(theGame.bottom, cell, stones)
            val newBottomM = theGame.bottomMancala
            Right(new Game(newTop, newTopM, newBottom, newBottomM, newNext))
          }
          case Bottom => {
            val newTop = updateInactiveRow(theGame.top, cell, stones)
            val newTopM = theGame.topMancala
            val newBottom = updateActiveRow(theGame.bottom, cell, stones)
            val newBottomM = updateMancala(theGame.bottomMancala, cell, stones)
            Right(new Game(newTop, newTopM, newBottom, newBottomM, newNext))
          }
        }

      }
    }
  }

  def endsInMancala(theGame: Game, cell: Int, stones: Int): Boolean = {
    val cycles = stones / 13
    val tail = stones - cycles * 13
    tail == 7 - cell
  }

  private[this] def getStones(theGame: Game, cell: Int): Either[String, Int] = {
    val max = theGame.top.length
    if (cell > max || cell < 1) {
      Left(s"Invalid move. Only numbers from 1 to $max are allowed.")
    } else {
      val stones = theGame.next match {
        case Top => theGame.top(cell - 1)
        case Bottom => theGame.bottom(cell - 1)
      }
      if (stones == 0) {
        Left(s"Invalid move. Cell #$cell is empty.")
      } else {
        Right(stones)
      }
    }
  }

  private[this] def updateActiveRow(theRow: List[Int], cell: Int, stones: Int): List[Int] = {
    theRow.zipWithIndex.map {
      case (stonesInCell, cellIndex) => {
        val cycles = stones / 13
        val tail = stones - cycles * 13
        if (cellIndex == cell - 1) {
          cycles
        } else {
          val index = if (cellIndex < cell - 1) {
            cellIndex + 13
          } else {
            cellIndex
          }
          val extraStone = if (tail > index - cell) 1 else 0
          stonesInCell + cycles + extraStone
        }
      }
    }
  }

  private[this] def updateMancala(theM: Int, cell: Int, stones: Int): Int = {
    theM + ((stones + cell + 5) / 12) // spooky formula, does it even work?
  }

  private[this] def updateInactiveRow(theRow: List[Int], cell: Int, stones: Int): List[Int] = {
    theRow.zipWithIndex.map {
      case (stonesInCell, cellIndex) => {
        val cycles = stones / 13
        val tail = stones - cycles * 13
        val extraStone = if (tail > cellIndex + 7 - cell) 1 else 0
        stonesInCell + cycles + extraStone
      }
    }
  }
}
