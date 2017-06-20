package mancala


class Game private ( val left: Array[Int]
                   , val leftMancala: Int
                   , val right: Array[Int]
                   , val rightMancala: Int
                   ) {

  override def toString : String = {
    val leftRow = left.mkString(" ")
    val rightRow = right.mkString(" ")
    s"L: $leftRow LM: $leftMancala\nR: $rightRow RM: $rightMancala"
  }

}


object Game {
  def create() = {
    new Game( Array.fill(6){4}, 0, Array.fill(6){4}, 0 )
  }
}
