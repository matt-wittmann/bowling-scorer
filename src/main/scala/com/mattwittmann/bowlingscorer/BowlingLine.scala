package com.mattwittmann.bowlingscorer

object BowlingLine {
  private[this] val TotalFrames = 10
  private[this] val LastFrame = 9
  type Score = Int
  type BowlingLine = Seq[Score]
  type Bonuses = (List[Int], List[Int])
  sealed class Roll
  object FirstRoll extends Roll
  object SecondRoll extends Roll
  object Over extends Roll
  abstract class BonusRoll extends Roll
  object LastBonusRoll extends BonusRoll
  object FirstBonusRoll extends BonusRoll
  private[this] val UnplayedFrames = Array.fill[Score](TotalFrames)(0)
  private[this] val NoBonuses = (Nil, Nil)
  case class State(currentFrame: Int = 0, roll: Roll = FirstRoll, bonuses: Bonuses = NoBonuses, frames: BowlingLine = UnplayedFrames)

  private[this] def calculatePinsKnockedDownAndCurrentFrameScore(lastState: State, roll: Char): (Int, Score) = {
    val lastCurrentFrameScore = lastState.frames(lastState.currentFrame)

    roll match {
      case 'X' => (10, lastCurrentFrameScore + 10)
      case '/' => (10 - lastCurrentFrameScore, 10)
      case '-' => (0, lastCurrentFrameScore)
      case digit => {
        val convertedDigit = digit.asDigit
        (convertedDigit, lastCurrentFrameScore + convertedDigit)
      }
    }
  }

  private[this] def calculateNextFrameAndRoll(lastState: State, roll: Char): (Int, Roll) = {
    val currentFrame = lastState.currentFrame
    val lastRoll = lastState.roll

    if ('X' == roll && currentFrame == LastFrame && lastRoll == FirstRoll)
      (LastFrame, FirstBonusRoll)
    else if (lastRoll == FirstBonusRoll || ('/' == roll && currentFrame == LastFrame && lastRoll == SecondRoll))
      (LastFrame, LastBonusRoll)
    else if (lastRoll == LastBonusRoll)
      (LastFrame, Over)
    else if (lastRoll == SecondRoll && currentFrame == LastFrame)
      (LastFrame, Over)
    else if (lastRoll == SecondRoll || 'X' == roll)
      (currentFrame + 1, FirstRoll)
    else
      (currentFrame, SecondRoll)
  }

  private[this] def calculateNextBonuses(lastState: State, roll: Char): Bonuses = {
    val currentFrame = lastState.currentFrame

    if (roll == 'X')
      (currentFrame :: lastState.bonuses._2, List(currentFrame))
    else if (roll == '/')
      (currentFrame :: lastState.bonuses._2, Nil)
    else
      (lastState.bonuses._2, Nil)
  }

  private[this] def calculateNextFrames(lastState: State, roll: Char): BowlingLine = {
    val (pinsKnockedDown, currentFrameScore) = calculatePinsKnockedDownAndCurrentFrameScore(lastState, roll)

    lastState.frames.zipWithIndex.map {
      case (score, index) => {
        if (index == lastState.currentFrame)
          currentFrameScore
        else if (lastState.bonuses._1.contains(index))
          score + pinsKnockedDown
        else
          score
      }
    }
  }

  def score(line: String): Score = {
    line.foldLeft[State](State()) { (lastState, roll) =>
      val (nextFrame, nextRoll) = calculateNextFrameAndRoll(lastState, roll)
      val nextBonuses = calculateNextBonuses(lastState, roll)
      val nextFrames = calculateNextFrames(lastState, roll)

      State(nextFrame, nextRoll, nextBonuses, nextFrames)
    }.frames.sum
  }
}
