package mancala


class Game private (val board: List[Int], val moves: List[Int], val bottomNext: Boolean) {
  val size = board.length / 2 - 1
  val lastIndex = size * 2 + 1
  val top = board.slice(size + 1, lastIndex)
  val topMancala = board(lastIndex)
  val bottom = board.slice(0, size)
  val bottomMancala = board(size)
  val turnedBoard = board.slice(size + 1, lastIndex + 1) ++ board.slice(0, size + 1)


  override def toString: String = {
    val topRow = top.reverse.mkString("\t")
    val bottomRow = bottom.mkString("\t")
    s"[ $topMancala ]\t$topRow\n\t$bottomRow\t[ $bottomMancala ]"
  }

  def getStones(cell: Int): Either[String, Int] = {
    if (cell > size || cell < 1) Left(s"Invalid cell #. Only numbers from 1 to $size are allowed.")
    else Right(if (bottomNext) bottom(cell - 1) else top(cell - 1))
  }

  def updateBoard(cell: Int, stones: Int): List[Int] = {
    val cycles = stones / lastIndex
    val tail = stones - cycles * lastIndex
    val activeBoard = if (bottomNext) board else turnedBoard
    val updateBoard = activeBoard.zipWithIndex.map {
      case (stonesInCell, cellIndex) => {
        if (cellIndex == lastIndex) {
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
    if (bottomNext) updateBoard else updateBoard.slice(size + 1, lastIndex + 1) ++ updateBoard.slice(0, size + 1)
  }

  def endsInMancala(cell: Int, stones: Int): Boolean = {
    val cycles = stones / lastIndex
    val tail = stones - cycles * lastIndex
    tail == size + 1 - cell
  }

  def move(cell: Int): Either[String, Game] = {
    getStones(cell) match {
      case Left(error) => Left(error)
      case Right(stones) => {
          if (stones == 0) {
            Left(s"Invalid move. Cell #$cell is empty.")
          } else {
            val newBoard = updateBoard(cell, stones)
            val newBottomNext = if (endsInMancala(cell, stones)) bottomNext else !bottomNext
            Right(new Game(newBoard, cell :: moves, newBottomNext))
          }
      }
    }
  }
}


object Game {

  def create6x6() = {
    val side = List.fill(6){4} ++ List(0)
    new Game(side ++ side, List(), true)
  }

  def create = create6x6

}
