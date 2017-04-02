package com.mizhi.nlp.stemmers.huskpaice

import com.mizhi.nlp.stemmers.huskpaice.RuleAction._

class RuleSetSpec extends UnitSpec {
  val rules = List(Rule("foo", Some("bar"), false, stop))

  describe("selectForEnding") {
    val ruleSet = new RuleSet(rules)

    it("returns a list of rules when the last char has an applicable list of rules") {
      ruleSet.selectForEnding("oo") should be(Some(rules))
    }

    it("returns none when there is no matching rule") {
      ruleSet.selectForEnding("ar") should be(None)
    }

    it("returns none when the string is empty") {
      ruleSet.selectForEnding("") should be(None)
    }
  }

  describe(".apply") {
    val ruleSet = RuleSet(rules:_*)

    it("should be a RuleSet") {
      ruleSet.isInstanceOf[RuleSet]
    }
  }
}
