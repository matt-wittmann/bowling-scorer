package com.mattwittmann.bowlingscorer

sealed trait Roll
case object FirstRoll extends Roll
case object SecondRoll extends Roll
case object Over extends Roll
trait BonusRoll extends Roll
case object LastBonusRoll extends BonusRoll
case object FirstBonusRoll extends BonusRoll