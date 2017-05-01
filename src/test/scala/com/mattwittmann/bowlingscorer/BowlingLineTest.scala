package com.mattwittmann.bowlingscorer

import org.scalatest.WordSpec

/**
  * Unit test for [[BowlingLine]].
  * See the [[http://codingdojo.org/kata/Bowling/ Coding Dojo Bowling Kata]] for rules and sample input.
  */
class BowlingLineTest extends WordSpec {
  "A BowlingLine" when {
    "supplied a line of rolls XXXXXXXXXXXX" should {
      "yield a score of 300" in {
        assert(BowlingLine.score("XXXXXXXXXXXX") === Right(300))
      }
    }
    "supplied a line of rolls 9-9-9-9-9-9-9-9-9-9-" should {
      "yield a score of 90" in {
        assert(BowlingLine.score("9-9-9-9-9-9-9-9-9-9-") === Right(90))
      }
    }
    "supplied a line of rolls 5/5/5/5/5/5/5/5/5/5/5" should {
      "yield a score of 150" in {
        assert(BowlingLine.score("5/5/5/5/5/5/5/5/5/5/5") === Right(150))
      }
    }
    "supplied a line of rolls --------------------" should {
      "yield a score of 0" in {
        assert(BowlingLine.score("--------------------") === Right(0))
      }
    }
    "supplied a line of rolls 44444444444444444444" should {
      "yield a score of 80" in {
        assert(BowlingLine.score("44444444444444444444") === Right(80))
      }
    }
    "supplied a line of rolls 47444444444444444444" should {
      "yield an error of TooManyPins" in {
        assert(BowlingLine.score("47444444444444444444") === Left(TooManyPins))
      }
    }
    "supplied a line of rolls 444444444444444444444" should {
      "yield an error of TooManyFrames" in {
        assert(BowlingLine.score("444444444444444444444") === Left(TooManyFrames))
      }
    }
    "supplied a line of rolls 5/5/5/5/5/5/5/5/5/5/5X" should {
      "yield an error of TooManyFrames" in {
        assert(BowlingLine.score("5/5/5/5/5/5/5/5/5/5/5X") === Left(TooManyFrames))
      }
    }
    "supplied a line of rolls XXXXXXXXXXXXX" should {
      "yield an error of TooManyFrames" in {
        assert(BowlingLine.score("XXXXXXXXXXXXX") === Left(TooManyFrames))
      }
    }
    "supplied a line of rolls 4A444444444444444444" should {
      "yield an error of InvalidInputCharacter('A')" in {
        assert(BowlingLine.score("4A444444444444444444") === Left(InvalidInputCharacter('A')))
      }
    }
    "supplied a line of rolls 4444444444" should {
      "yield an error of IncompleteGame" in {
        assert(BowlingLine.score("4444444444") === Left(IncompleteGame(5, 40)))
      }
    }
    "supplied a line of rolls 44444444444" should {
      "yield an error of IncompleteGame" in {
        assert(BowlingLine.score("44444444444") === Left(IncompleteGame(5, 40)))
      }
    }
    "supplied a line of rolls XXXXXXXXXXXX" should {
      "yield an error of IncompleteGame" in {
        assert(BowlingLine.score("XXXXXXXXXXX") === Left(IncompleteGame(9, 270)))
      }
    }
    "supplied a line of rolls XXXXXXXXXXX" should {
      "yield an error of IncompleteGame" in {
        assert(BowlingLine.score("XXXXXXXXXX") === Left(IncompleteGame(9, 260)))
      }
    }
    "supplied an empty line" should {
      "yield an error of IncompleteGame" in {
        assert(BowlingLine.score("") === Left(IncompleteGame(0, 0)))
      }
    }
    "supplied a null line" should {
      "yield an error of IncompleteGame" in {
        assert(BowlingLine.score(null) === Left(IncompleteGame(0, 0)))
      }
    }
  }
}
