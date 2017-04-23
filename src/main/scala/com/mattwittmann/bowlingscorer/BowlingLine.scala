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

  def score(line: String): Score = {
    line.foldLeft[State](State()) { (lastState, roll) =>
      val currentFrame = lastState.currentFrame
      val lastCurrentFrameScore = lastState.frames(currentFrame)
      val (pinsKnockedDown, currentFrameScore) = roll match {
        case 'X' => (10, lastCurrentFrameScore + 10)
        case '/' => (10 - lastCurrentFrameScore, 10)
        case '-' => (0, lastCurrentFrameScore)
        case digit => {
          val convertedDigit = digit.asDigit
          (convertedDigit, lastCurrentFrameScore + convertedDigit)
        }
      }

      val (nextFrame, nextRoll) =
        if ('X' == roll && currentFrame == LastFrame && lastState.roll == FirstRoll)
          (LastFrame, FirstBonusRoll)
        else if ('/' == roll && currentFrame == LastFrame && lastState.roll == SecondRoll)
          (LastFrame, LastBonusRoll)
        else if (lastState.roll == FirstBonusRoll)
          (LastFrame, LastBonusRoll)
        else if (lastState.roll == LastBonusRoll)
          (LastFrame, Over)
        else if (lastState.roll == SecondRoll && currentFrame == LastFrame)
          (LastFrame, Over)
        else if (lastState.roll == SecondRoll || 'X' == roll)
          (currentFrame + 1, FirstRoll)
        else
          (currentFrame, SecondRoll)

      val nextBonuses =
        if (roll == 'X')
          (currentFrame :: lastState.bonuses._2, List(currentFrame))
        else if (roll == '/')
          (currentFrame :: lastState.bonuses._2, Nil)
        else
          (lastState.bonuses._2, Nil)

      val nextFrames: BowlingLine = lastState.frames.zipWithIndex.map {
        case (score, index) => {
          if (index == currentFrame)
            currentFrameScore
          else if (lastState.bonuses._1.contains(index))
            score + pinsKnockedDown
          else
            score
        }
      }

      State(nextFrame, nextRoll, nextBonuses, nextFrames)
    }.frames.sum
  }
}
