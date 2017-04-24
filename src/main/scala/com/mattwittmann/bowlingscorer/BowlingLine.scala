package com.mattwittmann.bowlingscorer

/**
  * A line, or a game, of ten-pin bowling.
  * See [[http://codingdojo.org/kata/Bowling/ Coding Dojo Bowling Kata]].
  */
object BowlingLine {
  private[this] val TotalFrames = 10
  private[this] val LastFrame = 9
  private[this] val DecimalRadix = 10
  type Score = Int
  type BowlingLine = Seq[Score]
  type Bonuses = (List[Int], List[Int])
  sealed trait Roll
  case object FirstRoll extends Roll
  case object SecondRoll extends Roll
  case object Over extends Roll
  trait BonusRoll extends Roll
  case object LastBonusRoll extends BonusRoll
  case object FirstBonusRoll extends BonusRoll
  sealed trait InputError
  case class InvalidInputCharacter(character: Char) extends InputError
  case object TooManyPins extends InputError
  case object TooManyFrames extends InputError
  case class IncompleteGame(framesCompleted: Int, score: Score) extends InputError
  private[this] val UnplayedFrames = Array.fill[Score](TotalFrames)(0)
  private[this] val NoBonuses = (Nil, Nil)
  case class State(currentFrame: Int = 0, roll: Roll = FirstRoll, bonuses: Bonuses = NoBonuses, frames: BowlingLine = UnplayedFrames)

  private[this] def isBonusRoll(lastState: State) = List(FirstBonusRoll, LastBonusRoll).contains(lastState.roll)

  private[this] def calculatePinsKnockedDownAndCurrentFrameScore(lastState: State, roll: Char): Either[InputError, (Int, Score)] = {
    val lastCurrentFrameScore = lastState.frames(lastState.currentFrame)

    roll match {
      case 'X' => Right((10, lastCurrentFrameScore + 10))
      case '/' => Right((10 - lastCurrentFrameScore, 10))
      case '-' => Right((0, lastCurrentFrameScore))
      case digit => {
        val convertedDigit = Character.digit(digit, DecimalRadix)

        if (convertedDigit < 1)
          Left(InvalidInputCharacter(digit))
        else {

          val currentFrameScore = lastCurrentFrameScore + convertedDigit

          if ((isBonusRoll(lastState) && currentFrameScore > 30) ||
            (!isBonusRoll(lastState) && currentFrameScore > 9))
            Left(TooManyPins)
          else
            Right((convertedDigit, currentFrameScore))
        }
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

  private[this] def calculateNextFrames(lastState: State, roll: Char): Either[InputError, BowlingLine] = {
    calculatePinsKnockedDownAndCurrentFrameScore(lastState, roll).map {
      case (pinsKnockedDown, currentFrameScore) =>
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
  }

  /**
    * Scores a line of bowling.
    *
    * A line of bowling should follow this format:
    *
    * - `1-9` for a roll hitting one to nine pins
    * - `-` A hyphen represents a miss or zero pins hit in the roll
    * - `/` represents a spare, representing knocking down all remaining pins on the second roll of the frame
    * - `X` represents a strike, knocking down all ten pins on the first roll of a frame
    *
    * @param line The line of bowling in a particular format
    * @return The final score for the given line of bowling
    */
  def score(line: String): Either[InputError, Score] = {
    line.foldLeft[Either[InputError, State]](Right(State())) { (eitherLastState, roll) =>
      eitherLastState.flatMap { lastState =>
        if (lastState.roll == Over)
          Left(TooManyFrames)
        else {
          val (nextFrame, nextRoll) = calculateNextFrameAndRoll(lastState, roll)
          val nextBonuses = calculateNextBonuses(lastState, roll)
          calculateNextFrames(lastState, roll).map { nextFrames =>
            State(nextFrame, nextRoll, nextBonuses, nextFrames)
          }
        }
      }
    }.flatMap { state =>
      if (state.roll == Over)
        Right(state)
      else
        Left(IncompleteGame(state.currentFrame, state.frames.take(state.currentFrame).sum))
    }.map(_.frames.sum)
  }
}
