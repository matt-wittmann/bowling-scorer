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
        assert(BowlingLine.score("XXXXXXXXXXXX") === 300)
      }
    }
    "supplied a line of rolls 9-9-9-9-9-9-9-9-9-9-" should {
      "yield a score of 90" in {
        assert(BowlingLine.score("9-9-9-9-9-9-9-9-9-9-") === 90)
      }
    }
    "supplied a line of rolls 5/5/5/5/5/5/5/5/5/5/5" should {
      "yield a score of 150" in {
        assert(BowlingLine.score("5/5/5/5/5/5/5/5/5/5/5") === 150)
      }
    }
    "supplied a line of rolls --------------------" should {
      "yield a score of 0" in {
        assert(BowlingLine.score("--------------------") === 0)
      }
    }
    "supplied a line of rolls 44444444444444444444" should {
      "yield a score of 80" in {
        assert(BowlingLine.score("44444444444444444444") === 80)
      }
    }
  }
}
