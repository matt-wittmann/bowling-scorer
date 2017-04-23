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
        assert(300 === BowlingLine.score("XXXXXXXXXXXX"))
      }
    }
    "supplied a line of rolls 9-9-9-9-9-9-9-9-9-9-" should {
      "yield a score of 90" in {
        assert(90 === BowlingLine.score("9-9-9-9-9-9-9-9-9-9-"))
      }
    }
    "supplied a line of rolls 5/5/5/5/5/5/5/5/5/5/5" should {
      "yield a score of 150" in {
        assert(150 === BowlingLine.score("5/5/5/5/5/5/5/5/5/5/5"))
      }
    }
    "supplied a line of rolls --------------------" should {
      "yield a score of 0" in {
        assert(0 === BowlingLine.score("--------------------"))
      }
    }
  }
}
