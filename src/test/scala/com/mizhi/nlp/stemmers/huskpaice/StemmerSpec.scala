package com.mizhi.nlp.stemmers.huskpaice

import com.mizhi.nlp.stemmers.huskpaice.RuleAction._

class StemmerSpec extends UnitSpec {
  describe("stem") {
    val rules = List(
      Rule("baz", Some("foo"), false, continue),
      Rule("foo", Some("bar"), false, stop)
    )

    val ruleSet = RuleSet(rules:_*)
    val stemmer = Stemmer(ruleSet)

    it("returns unaltered stem when rules are not applicable") {
      stemmer.stem("foono") should equal("foono")
    }

    it("returns stemmed word when rules are applicable") {
      stemmer.stem("foofoo") should equal("foobar")
    }
  }

  describe("selectAndApplyRules") {
    val rules = List(
      Rule("baz", Some("foo"), false, continue),
      Rule("foo", Some("bar"), false, stop)
    )

    val ruleSet = RuleSet(rules:_*)
    val stemmer = Stemmer(ruleSet)

    it("returns unaltered stem when rules are not applicable") {
      val es = StemmingState("foono", true, None)
      stemmer.selectAndApplyRules(es) should be(es.copy(nextAction = Some(stop)))
    }

    it("chains continued rules") {
      val es = StemmingState("foobaz", true, None)
      stemmer.selectAndApplyRules(es) should be(es.copy("foobar", false, nextAction = Some(stop)))
    }
  }

  describe("applyRules") {
    val ruleSet = RuleSet()
    val stemmer = Stemmer(ruleSet)
    val state = StemmingState("foo", true, None)

    it("returns stop state when there are no more rules to execute") {
      stemmer.applyRules(state, List.empty[Rule]) should be(state.copy(nextAction = Some(stop)))
    }

    describe("returns when") {
      it("a rule applies and continues") {
        val bsRules = List(Rule("foo", Some("bar"), false, continue))
        stemmer.applyRules(state, bsRules) should be(StemmingState("bar", false, nextAction = Some(continue)))
      }

      it("a rule applies and stops") {
        val bsRules = List(Rule("foo", Some("bar"), false, stop))
        stemmer.applyRules(state, bsRules) should be(StemmingState("bar", false, nextAction = Some(stop)))
      }

      it("no rule applies") {
        val bsRules = List(Rule("nofoo", Some("bar"), false, stop))
        stemmer.applyRules(state, bsRules) should be(StemmingState("foo", true, nextAction = Some(stop)))
      }

      it("cycles through rules until reaches applicable rule") {
        val bsRules = List(
          Rule("dontmatch", Some("doesntmatter"), false, stop),
          Rule("foo", Some("bar"), false, stop))
        stemmer.applyRules(state, bsRules) should be(StemmingState("bar", false, nextAction = Some(stop)))
      }
    }
  }
}
