package com.mattwittmann.bowlingscorer

sealed trait InputError
case class InvalidInputCharacter(character: Char) extends InputError
case object TooManyPins extends InputError
case object TooManyFrames extends InputError
case class IncompleteGame(framesCompleted: Int, score: Score) extends InputError
