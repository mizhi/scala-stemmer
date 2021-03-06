package com.mizhi.nlp.stemmers.huskpaice

object RuleAction extends scala.Enumeration {
  type RuleAction = Value
  val continue, stop = Value
}
import com.mizhi.nlp.stemmers.huskpaice.RuleAction._

case class StemmingState(word: String, intact: Boolean, nextAction: Option[RuleAction.RuleAction])

trait RuleExecutor {
  def apply(state: StemmingState): StemmingState
}

case class Rule(suffix: String, append: Option[String], intactOnly: Boolean, nextAction: RuleAction) extends RuleExecutor {
  override def apply(state: StemmingState): StemmingState = {
    lazy val stemmed = applyStringTransform(state.word)
    if (ruleApplies(state) && stemAcceptable(stemmed)) {
      StemmingState(stemmed, false, Some(nextAction))
    } else {
      state
    }
  }

  // The original algorithm specified the transforms using a deletion amount
  // and subsequent append. This algorithm uses some of the nicer string
  // processing functions we have available these days.
  protected[huskpaice] def applyStringTransform(word: String): String = {
    append.fold(word)(word.stripSuffix(suffix).concat)
  }

  protected[huskpaice] def ruleApplies(state: StemmingState): Boolean = {
    endingMatches(state.word) && intactnessIsGood(state)
  }

  // A Rule only applies if the word has a matching suffix
  protected[huskpaice] def endingMatches(word: String) = word.endsWith(suffix)

  // Husk-Paice won't allow a Rule to apply if it mandates the word is intact at the start
  protected[huskpaice] def intactnessIsGood(state: StemmingState) = {
    !intactOnly || (intactOnly && state.intact)
  }

  // There are two oddball acceptability requirements.
  // 1. if starts with a vowel, then must contain two letters after stemming
  // 2. if starts with consonant, then must contain three letters after stemming and
  // one must be vowel or y
  protected val Vowels = Set('a', 'e', 'i', 'o', 'u')
  protected val VowelsAndY = Vowels + 'y'
  protected[huskpaice] def stemAcceptable(word: String): Boolean = {
    word.headOption.fold(false)(_ match {
      case x if Vowels.contains(x) => word.length >= 2
      case _ => (word.length >= 3) && (word.count(VowelsAndY) > 0)
    })
  }
}

object Rule {
  // This allows us to gracefully bottom out when we run out of potential rules to apply
  // when a given word can't be stemmed by any of the potential rules.
  val nullRule = new RuleExecutor {
    override def apply(state: StemmingState): StemmingState = state.copy(nextAction = Some(stop))
  }
}
