package com.mattwittmann.bowlingscorer

/**
  * A line, or a game, of ten-pin bowling.
  * See [[http://codingdojo.org/kata/Bowling/ Coding Dojo Bowling Kata]].
  */
object BowlingLine {
  private val NullLineAsIncompleteGameError = Left(IncompleteGame(0, 0))
  private val InitialGameState = Right(State())

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
    * @return The final score for the given line of bowling or the [[InputError]]
    */
  def score(line: String): Either[InputError, Score] = {
    if (line == null)
      NullLineAsIncompleteGameError
    else
      scoreNonNullLine(line)
  }

  private def scoreNonNullLine(line: String) = {
    val finalState = evaluateStates(line)
    val finalStateOrIncompleteGame = checkGameCompleted(finalState)

    sumAllFrames(finalStateOrIncompleteGame)
  }

  private def evaluateStates(line: String) = {
    line.foldLeft[Either[InputError, State]](InitialGameState) { (stateOrError, roll) =>
      stateOrError.flatMap { state =>
        if (state.isGameOver)
          Left(TooManyFrames)
        else
          nextState(roll, state)
      }
    }
  }

  private def nextState(roll: Char, lastState: State) = {
    val (nextFrame, nextRoll) = calculateNextFrameAndRoll(lastState, roll)
    val nextBonuses = calculateNextBonuses(lastState, roll)
    calculateNextFrames(lastState, roll).map { nextFrames =>
      State(nextFrame, nextRoll, nextBonuses, nextFrames)
    }
  }

  private def checkGameCompleted(nextState: Either[InputError, State]) = {
    nextState.flatMap { state =>
      if (state.isGameOver)
        Right(state)
      else
        calculateScoreForIncompleteGame(state)
    }
  }

  private def sumAllFrames(finalStateOrIncompleteGame: Either[InputError, State]) = finalStateOrIncompleteGame.map(_.frames.sum)

  private def calculateScoreForIncompleteGame(state: State) = {
    val currentFrame = state.currentFrame
    val completedFrames = state.frames.take(currentFrame)
    Left(IncompleteGame(currentFrame, completedFrames.sum))
  }

  private def isBonusRoll(lastState: State) = BonusRolls.contains(lastState.roll)

  private def calculateNextFrameAndRoll(lastState: State, roll: Char): (Int, Roll) = {
    val currentFrame = lastState.currentFrame
    val lastRoll = lastState.roll

    if (isStrikeForEndGameBonusRolls(roll, currentFrame, lastRoll))
      (LastFrame, FirstBonusRoll)
    else if (isLastBonusRollNext(roll, currentFrame, lastRoll))
      (LastFrame, LastBonusRoll)
    else if (isLastBonusRollGameOver(lastRoll))
      (LastFrame, Over)
    else if (isNoBonusGameOver(currentFrame, lastRoll))
      (LastFrame, Over)
    else if (isNewFrameNext(roll, lastRoll))
      (currentFrame + 1, FirstRoll)
    else
      (currentFrame, SecondRoll)
  }

  private def isStrikeForEndGameBonusRolls(roll: Char, currentFrame: Int, lastRoll: Roll) = {
    'X' == roll && currentFrame == LastFrame && lastRoll == FirstRoll
  }

  private def isLastBonusRollNext(roll: Char, currentFrame: Int, lastRoll: Roll) = {
    lastRoll == FirstBonusRoll || ('/' == roll && currentFrame == LastFrame && lastRoll == SecondRoll)
  }

  private def isLastBonusRollGameOver(lastRoll: Roll) = lastRoll == LastBonusRoll

  private def isNoBonusGameOver(currentFrame: Int, lastRoll: Roll) = lastRoll == SecondRoll && currentFrame == LastFrame

  private def isNewFrameNext(roll: Char, lastRoll: Roll) = lastRoll == SecondRoll || 'X' == roll

  private def calculateNextBonuses(state: State, roll: Char): Bonuses = {
    val currentFrame = state.currentFrame

    if (roll == 'X')
      (currentFrame :: state.bonuses._2, List(currentFrame))
    else if (roll == '/')
      (currentFrame :: state.bonuses._2, Nil)
    else
      (state.bonuses._2, Nil)
  }

  private def calculateNextFrames(state: State, roll: Char): Either[InputError, BowlingLine] = {
    calculatePinsKnockedDownAndCurrentFrameScore(state, roll).map {
      case (pinsKnockedDown, currentFrameScore) =>
        state.frames.zipWithIndex.map {
          case (score, index) => addPointsToFrames(state, pinsKnockedDown, currentFrameScore, score, index)
        }
    }
  }

  private def addPointsToFrames(state: State, pinsKnockedDown: Score, currentFrameScore: Score, score: Score, index: Score) = {
    // Update the points for the frame in play
    if (index == state.currentFrame)
      currentFrameScore
    // Update the points to a previous frame due to a bonus
    else if (state.bonuses._1.contains(index))
      score + pinsKnockedDown
    // The frame score should not otherwise be changed
    else
      score
  }

  private def calculatePinsKnockedDownAndCurrentFrameScore(lastState: State, roll: Char): Either[InputError, (Int, Score)] = {
    val lastCurrentFrameScore = lastState.frames(lastState.currentFrame)

    roll match {
      case 'X' => Right((10, lastCurrentFrameScore + 10))
      case '/' => Right((10 - lastCurrentFrameScore, 10))
      case '-' => Right((0, lastCurrentFrameScore))
      case digit => calculateFrameScoreForSomePinsKnockedDown(lastState, lastCurrentFrameScore, digit)
    }
  }

  private def calculateFrameScoreForSomePinsKnockedDown(lastState: State, lastCurrentFrameScore: Score, digit: Char) = {
    val convertedDigit = Character.digit(digit, DecimalRadix)

    if (convertedDigit < 1)
      Left(InvalidInputCharacter(digit))
    else
      calculateFrameScoreUsingValidDigit(lastState, lastCurrentFrameScore, convertedDigit)
  }

  private def calculateFrameScoreUsingValidDigit(lastState: State, lastCurrentFrameScore: Score, pinsKnockedDown: Score) = {
    val currentFrameScore = lastCurrentFrameScore + pinsKnockedDown

    if (exceedsBonusMaxPins(lastState, currentFrameScore) ||
      exceedsNonBonusMaxPins(lastState, currentFrameScore))
      Left(TooManyPins)
    else
      Right((pinsKnockedDown, currentFrameScore))
  }

  private def exceedsBonusMaxPins(lastState: State, currentFrameScore: Score) = isBonusRoll(lastState) && currentFrameScore > 30

  private def exceedsNonBonusMaxPins(lastState: State, currentFrameScore: Score) = !isBonusRoll(lastState) && currentFrameScore > 9
}
