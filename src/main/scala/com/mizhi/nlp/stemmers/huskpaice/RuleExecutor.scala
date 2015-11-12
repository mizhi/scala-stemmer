package com.mizhi.nlp.stemmers.huskpaice

case class Word(text: String, intact: Boolean)

object RuleAction extends scala.Enumeration {
  type RuleAction = Value
  val continue, stop = Value
}

import com.mizhi.nlp.stemmers.huskpaice.RuleAction._

case class ExecutionState(word: Word, nextAction: Option[RuleAction])

trait RuleExecutor {
  def execute(state: ExecutionState): ExecutionState
}
