package mancala

import org.scalatest.{FlatSpec, Matchers}

class MancalaMainLoopSpec extends FlatSpec with Matchers {
  "The MancalaMainLoop object" should "say 'Hello, this is beginning of a great game!\nL: 4 4 4 4 4 4 LM: 0\nR: 4 4 4 4 4 4 RM: 0'" in {
    MancalaMainLoop.main(Array[String]())
  }
}
