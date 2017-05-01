package com.mattwittmann

package object bowlingscorer {
  val TotalFrames = 10
  val LastFrame = 9
  val DecimalRadix = 10

  type Score = Int
  type BowlingLine = Seq[Score]
  type Bonuses = (List[Int], List[Int])

  val NoBonuses: Bonuses = (Nil, Nil)

  private val UnplayedFrames = Seq.fill[Score](TotalFrames)(0)

  case class State(currentFrame: Int = 0,
                   roll: Roll = FirstRoll,
                   bonuses: Bonuses = NoBonuses,
                   frames: BowlingLine = UnplayedFrames) {
    def isGameOver() = roll == Over
  }

  val BonusRolls = List(FirstBonusRoll, LastBonusRoll)
}
