package com.mattwittmann.bowlingscorer

object BowlingLine {
  def score(line: String): Int = line.map {
    case '-' => 0
    case roll => roll.asDigit
  }.sum
}
