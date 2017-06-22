package mancala


class Game private ( val top: List[Int]
                   , val topMancala: Int
                   , val bottom: List[Int]
                   , val bottomMancala: Int
                   , val next: Side
                   ) {

  override def toString: String = {
    val topRow = top.mkString(" ")
    val bottomRow = bottom.mkString(" ")
    s"L: $topRow LM: $topMancala\nR: $bottomRow RM: $bottomMancala\nNM: $next"
  }

}


sealed abstract class Side() {}
case object Top extends Side() {
  override def toString: String = { "Top" }
}
case object Bottom extends Side() {
  override def toString: String = { "Bottom" }
}


object Game {
  def create() = {
    new Game(List.fill(6){4}, 0, List.fill(6){4}, 0, Bottom)
  }

  def validateMove(aGame: Game, i: Int): Option[String] = {
    val next = aGame.next
    val top = aGame.top
    val bottom = aGame.bottom
    val max = top.length
    if (i > max || i < 1) {
      Some(s"Invalid move. Only numbers from 1 to $max are allowed.")
    } else {
      val cell = next match {
        case Top => {
          top(i - 1)
        }
        case Bottom => {
          bottom(i - 1)
        }
      }
      if (cell == 0) {
        Some(s"Invalid move. Cell #$i is empty.")
      } else {
        None
      }
    }
  }

  def move(aGame: Game, i: Int) : Either[(String, Game), Game] = {
    validateMove(aGame, i) match {
      case Some(error) => Left(error, aGame)
      case _ => Right(new Game(List.fill(6){4}, 0, List(4, 4, 0, 5, 5, 5), 1, Bottom)) // Fake move
    }
  }
}
