package com.mizhi.nlp.stemmers.huskpaice

object RuleAction extends scala.Enumeration {
  type RuleAction = Value
  val continue, stop = Value
}

case class StemmingState(word: String, intact: Boolean, nextAction: Option[RuleAction.RuleAction])

trait RuleExecutor {
  def execute(state: StemmingState): StemmingState
}
