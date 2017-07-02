package mancala


class Game private (val board: List[Int], val moves: List[Int], val bottomNext: Boolean) {
  val size = board.length / 2 - 1
  private[this] val lastIndex = size * 2 + 1
  val top = board.slice(size + 1, lastIndex)
  val topMancala = board(lastIndex)
  val bottom = board.slice(0, size)
  val bottomMancala = board(size)

  def ended = top == rowOf(0) || bottom == rowOf(0)

  override def toString: String = {
    val movesRow = moves.reverse.mkString(" ")
    val topRow = top.reverse.mkString("\t")
    val bottomRow = bottom.mkString("\t")
    s"$movesRow\n[ $topMancala ]\t$topRow\n\t$bottomRow\t[ $bottomMancala ]"
  }

  def getStones(cell: Int): Either[String, Int] = {
    if (cell > size || cell < 1) Left(s"Invalid cell #. Only numbers from 1 to $size are allowed.")
    else Right(if (bottomNext) bottom(cell - 1) else top(cell - 1))
  }

  def move(cell: Int): Either[String, Game] = {
    for {
      stones <- getStones(cell)
      game <- movedGame(cell, stones)
    } yield game
  }

  private[this] def updateBoard(cell: Int, stones: Int): List[Int] = {
    def turnBoard(b: List[Int]): List[Int] = {
      b.slice(size + 1, lastIndex + 1) ++ b.slice(0, size + 1)
    }
    val activeBoard = if (bottomNext) board else turnBoard(board)
    val updatedBoard = reflectMove(activeBoard, cell, stones)
    val conqueredBoard = conquering(cell, stones) match {
      case Some(index) => {
        reflectConquer(updatedBoard, index)
      }
      case None => updatedBoard
    }
    if (bottomNext) conqueredBoard else turnBoard(conqueredBoard)
  }

  private[this] def movedGame(cell: Int, stones: Int): Either[String, Game] = {
    def endsInMancala(cell: Int, stones: Int): Boolean = {
      val (_, tail) = cyclesAndTail(stones)
      tail == size + 1 - cell
    }
    if (stones == 0) {
      Left(s"Invalid move. Cell #$cell is empty.")
    } else {
      val newBoard = updateBoard(cell, stones)
      val newBottomNext = if (endsInMancala(cell, stones)) bottomNext else !bottomNext
      val newMoves = cell :: moves
      val game = new Game(newBoard, newMoves, newBottomNext)
      if (game.ended) {
        val topScore = game.topMancala + game.top.fold(0)(_ + _)
        val bottomScore = game.bottomMancala + game.bottom.fold(0)(_ + _)
        Right(new Game(rowOf(0) ++ List(bottomScore) ++ rowOf(0) ++ List(topScore), newMoves, bottomNext))
      } else {
        Right(game)
      }
    }
  }

  private[this] def reflectMove(board: List[Int], cell: Int, stones: Int): List[Int] = {
    val (cycles, tail) = cyclesAndTail(stones)
    board.zipWithIndex.map {
      case (stonesInCell, cellIndex) => {
        if (cellIndex == lastIndex) { // Don't add stones to oposite mancala
           stonesInCell
        } else {
          if (cellIndex == cell - 1) {
            cycles
          } else {
            val index = if (cellIndex < cell - 1) {
              cellIndex + lastIndex
            } else {
              cellIndex
            }
            val extraStone = if (tail > index - cell) 1 else 0
            stonesInCell + cycles + extraStone
          }
        }
      }
    }
  }

  private[this] def conquering(cell: Int, stones: Int): Option[Int] = {
    val activeSide = if (bottomNext) bottom else top
    val (_, tail) = cyclesAndTail(stones)
    val cellIndex = cell - 1
    val distToM = size - cellIndex
    val index = if (tail < distToM) {
      cellIndex + tail
    } else if (tail > size + 1 + distToM) {
      cellIndex + tail - size
    } else {
      -1
    }
    if (index > 0 && activeSide(index) == 0) Some(index)
    else None
  }

  private[this] def reflectConquer(board: List[Int], index: Int): List[Int] = {
    val opositeIndex = size * 2 - index
    val opositeStones = board(opositeIndex)
    val topIndex = opositeIndex - size - 1
    val bottomM = board(size) + opositeStones + 1
    val bottom = board.slice(0, size).updated(index, 0) ++ List(bottomM)
    val top = board.slice(size + 1, lastIndex + 1).updated(topIndex, 0)
    bottom ++ top
  }

  // TODO: weird helper method. Can we get rid of it?
  private[this] def cyclesAndTail(stones: Int): (Int, Int) = {
    val cycles = stones / lastIndex
    val tail = stones - cycles * lastIndex
    (cycles, tail)
  }

  private[this] def rowOf(x: Int): List[Int] = List.fill(size){ x }
}


object Game {

  def create6x6() = {
    val side = List.fill(6){4} ++ List(0)
    new Game(side ++ side, List(), true)
  }

  def create = create6x6

}
