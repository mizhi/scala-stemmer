package com.mizhi.nlp.stemmers.huskpaice

import com.mizhi.nlp.stemmers.huskpaice.RuleAction._

class RuleSetSpec extends UnitSpec {

  describe("selectForEnding") {
    val rules = List(Rule("foo", Some("bar"), false, stop))
    val ruleSet = RuleSet(rules:_*)

    it("returns a list of rules when the last char has an applicable list of rules") {
      ruleSet.selectForEnding("oo") should be(Some(rules))
    }

    it("returns none when there is no matching rule") {
      ruleSet.selectForEnding("ar") should be(None)
    }
  }

  describe("execute") {
    val rules = List(
      Rule("baz", Some("foo"), false, continue),
      Rule("foo", Some("bar"), false, stop)
    )

    val ruleSet = RuleSet(rules:_*)

    it("returns unaltered stem when rules are not applicable") {
      val es = ExecutionState(Word("foono", true), None)
      ruleSet.execute(es) should be(es.copy(nextAction = Some(stop)))
    }

    it("chains continued rules") {
      val es = ExecutionState(Word("foobaz", true), None)
      ruleSet.execute(es) should be(es.copy(Word("foobar", false), nextAction = Some(stop)))
    }
  }


  describe("executeRules") {
    val ruleSet = RuleSet()
    val state = ExecutionState(Word("foo", true), None)

    it("returns stop state when there are no more rules to execute") {
      ruleSet.executeRules(state, List.empty[Rule]) should be(state.copy(nextAction = Some(stop)))
    }

    describe("returns when") {
      it("a rule applies and continues") {
        val bsRules = List(Rule("foo", Some("bar"), false, continue))
        ruleSet.executeRules(state, bsRules) should be(ExecutionState(Word("bar", false), nextAction = Some(continue)))
      }

      it("a rule applies and stops") {
        val bsRules = List(Rule("foo", Some("bar"), false, stop))
        ruleSet.executeRules(state, bsRules) should be(ExecutionState(Word("bar", false), nextAction = Some(stop)))
      }

      it("no rule applies") {
        val bsRules = List(Rule("nofoo", Some("bar"), false, stop))
        ruleSet.executeRules(state, bsRules) should be(ExecutionState(Word("foo", true), nextAction = Some(stop)))
      }

      it("cycles through rules until reaches applicable rule") {
        val bsRules = List(
          Rule("dontmatch", Some("doesntmatter"), false, stop),
          Rule("foo", Some("bar"), false, stop))
        ruleSet.executeRules(state, bsRules) should be(ExecutionState(Word("bar", false), nextAction = Some(stop)))
      }
    }
  }
}