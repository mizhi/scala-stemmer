package com.mizhi.nlp.stemmers.huskpaice

import com.mizhi.nlp.stemmers.huskpaice.RuleAction._
import scala.annotation.tailrec

class Stemmer(ruleSet: RuleSet) extends RuleExecutor {
  // This allows us to gracefully bottom out when we run out of potential rules to apply
  // when a given word can't be stemmed by any of the potential rules.
  protected val nullRule = new RuleExecutor {
    override def execute(state: StemmingState): StemmingState = state.copy(nextAction = Some(stop))
  }

  def stem(word: String): String = execute(StemmingState(word, true, None)).word

  override def execute(state: StemmingState): StemmingState = {
    ruleSet.selectForEnding(state.word).fold(state)(
      executeRules(state, _) match {
        case stemmed @ StemmingState(_, _, Some(`continue`)) => execute(stemmed.copy(nextAction = None)) // rule was applied with continue
        case finalResult => finalResult // Return what we've got
      })
  }

  @tailrec
  final protected[huskpaice] def executeRules(state: StemmingState, rules: Seq[Rule]): StemmingState = {
    rules.headOption.getOrElse(nullRule).execute(state) match {
      case unstemmed @ StemmingState(_, _, None) => executeRules(unstemmed, rules.tail) // rule did not apply, move to next rule
      case stemmed => stemmed // Rule was applied, don't try any more rules and return the result
    }
  }
}

object Stemmer {
  def apply(ruleSet: RuleSet): Stemmer = new Stemmer(ruleSet)
}
