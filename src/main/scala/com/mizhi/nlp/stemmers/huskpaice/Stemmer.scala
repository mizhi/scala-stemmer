package com.mizhi.nlp.stemmers.huskpaice

import com.mizhi.nlp.stemmers.huskpaice.RuleAction._

import scala.annotation.tailrec

class Stemmer(ruleSet: RuleSet) {
  def stem(word: String): String = selectAndApplyRules(StemmingState(word, true, None)).word

  final protected[huskpaice] def selectAndApplyRules(state: StemmingState): StemmingState = {
    ruleSet.selectForEnding(state.word).fold(state)(
      applyRules(state, _) match {
        case stemmed @ StemmingState(_, _, Some(`continue`)) => selectAndApplyRules(stemmed.copy(nextAction = None)) // rule was applied with continue
        case finalResult => finalResult // Return what we've got
      })
  }

  @tailrec
  final protected[huskpaice] def applyRules(state: StemmingState, rules: Seq[Rule]): StemmingState = {
    rules.headOption.getOrElse(Rule.nullRule)(state) match {
      case unstemmed @ StemmingState(_, _, None) => applyRules(unstemmed, rules.tail) // rule did not apply, move to next rule
      case stemmed => stemmed // Rule was applied, don't try any more rules and return the result
    }
  }
}

object Stemmer {
  def apply(ruleSet: RuleSet): Stemmer = new Stemmer(ruleSet)
}
