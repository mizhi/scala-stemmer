package com.mizhi.nlp.stemmers.huskpaice

import com.mizhi.nlp.stemmers.huskpaice.RuleAction._
import org.mockito.Mockito.{doReturn, spy}

class RuleSpec extends UnitSpec {
  val rule = Rule("suffix", None, true, stop)

  describe("apply") {
    val state = StemmingState("asuffix", true, None)

    describe("when rule is applied") {
      val spiedRule = spy(rule.copy(append=Some("thing")))

      doReturn(true, Array[Object](): _*).when(spiedRule).ruleApplies(state)
      doReturn(true, Array[Object](): _*).when(spiedRule).stemAcceptable(state.word)

      it("removes suffix") {
        spiedRule(state).word should be("athing")
      }

      it("sets intact to false") {
        spiedRule(state).intact should be(false)
      }

      it("sets the nextAction to the rule's action") {
        spiedRule(state).nextAction should be (Some(spiedRule.nextAction))
      }
    }

    describe("when rule is not applied") {
      val spiedRule = spy(rule.copy(append=Some("thing")))
      doReturn(false, Array[Object](): _*).when(spiedRule).ruleApplies(state)

      it("returns same state") {
        spiedRule(state) should be(state)
      }
    }
  }

  describe("applyStringTransform") {
    describe("when append is None") {
      it("leaves stem alone") {
        rule.applyStringTransform("prefixwordsuffix") should be("prefixwordsuffix")
      }
    }

    describe("when append is the empty string") {
      it("strips suffix") {
        rule.copy(append=Some("")).applyStringTransform("prefixwordsuffix") should be("prefixword")
      }

    }

    describe("when append is not empty") {
      it("replaces suffix") {
        rule.copy(append=Some("newstring")).applyStringTransform("prefixwordsuffix") should be("prefixwordnewstring")
      }
    }
  }

  describe("endingMatches") {
    it("detects when ending matches suffix") {
      rule.endingMatches("somefoolstringsuffix") should be(true)
    }

    it("detects when ending doesn't match suffix") {
      rule.endingMatches("somefoolstring") should be(false)
    }
  }

  describe("intactnessIsGood") {
    val intactState = StemmingState("foo", true, None)
    val modifiedStem = intactState.copy(intact=false)

    describe("when rule requires the word to be intact") {
      it("is true when stem is intact") {
        rule.intactnessIsGood(intactState) should be(true)
      }

      it("is false when stem is not intact") {
        rule.intactnessIsGood(modifiedStem) should be(false)
      }
    }

    describe("when rule does not require the word to be intact") {
      val modifiedRule = rule.copy(intactOnly = false)

      it("is true when stem is intact") {
        modifiedRule.intactnessIsGood(intactState) should be(true)
      }

      it("is true when stem is not intact") {
        modifiedRule.intactnessIsGood(modifiedStem) should be(true)
      }
    }
  }

  describe("stemAcceptable") {
    describe("when word starts with vowel") {
      it("returns true when length is >= 2") {
        rule.stemAcceptable("ace") should be(true)
      }

      it("returns false when length is < 2") {
        rule.stemAcceptable("a") should be(false)
      }

      it("works with empty string") {
        rule.stemAcceptable("") should be(false)
      }
    }

    describe("when word starts with consonant") {
      it("returns true when length is >= 3 and has a vowel") {
        rule.stemAcceptable("cad") should be(true)
      }

      it("returns true when vowel is a y") {
        rule.stemAcceptable("cry") should be(true)
      }

      it("returns false when length is < 3") {
        rule.stemAcceptable("cr") should be (false)
      }

      it("returns false when there are no vowels") {
        rule.stemAcceptable("str") should be (false)
      }
    }
  }
}
