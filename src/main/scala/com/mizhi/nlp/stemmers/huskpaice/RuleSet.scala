package com.mizhi.nlp.stemmers.huskpaice

import com.mizhi.nlp.stemmers.huskpaice.RuleAction._

import scala.annotation.tailrec
import scala.collection.immutable.{Map, Seq}

class RuleSet(rules: Seq[Rule]) extends RuleExecutor {
  // Rules are keyed on the last character of the suffix.
  protected val rulesBySuffix: Map[String, Seq[Rule]] = rules.groupBy(_.suffix.last.toString)

  // This allows us to gracefully bottom out when we run out of potential rules to apply
  // when a given word can't be stemmed by any of the potential rules.
  protected val nullRule = new RuleExecutor {
    override def execute(state: StemmingState): StemmingState = state.copy(nextAction = Some(stop))
  }

  def stem(word: String): String = execute(StemmingState(Word(word, true), None)).word.text

  protected[huskpaice] def selectForEnding(s: String): Option[Seq[Rule]] = rulesBySuffix.get(s.lastOption.getOrElse("").toString)

  override def execute(state: StemmingState): StemmingState = {
    selectForEnding(state.word.text).fold(state)(
      executeRules(state, _) match {
        case stemmed @ StemmingState(_, Some(`continue`)) => execute(stemmed.copy(nextAction = None)) // rule was applied with continue
        case finalResult => finalResult // Return what we've got
      })
  }

  @tailrec
  final protected[huskpaice] def executeRules(state: StemmingState, rules: Seq[Rule]): StemmingState = {
    rules.headOption.getOrElse(nullRule).execute(state) match {
      case unstemmed @ StemmingState(_, None) => executeRules(unstemmed, rules.tail) // rule did not apply, move to next rule
      case stemmed => stemmed // Rule was applied, don't try any more rules and return the result
    }
  }
}

object RuleSet {
  def apply(rules: Rule*) = {
    new RuleSet(rules.toList)
  }

  lazy val Erase = Some("")
  lazy val english: RuleSet = RuleSet(
    Rule("ia", Erase, true, stop),                  //    "ai*2.",     # -ia > -   if intact
    Rule("a", Erase, true, stop),                   //    "a*1.",      # -a > -    if intact
    Rule("bb", Some("b"), false, stop),             //    "bb1.",      # -bb > -b
    Rule("ytic", Some("ys"), false, stop),          //    "city3s.",   # -ytic > -ys
    Rule("ic", Erase, false, continue),             //    "ci2>",      # -ic > -
    Rule("nc", Some("nt"), false, continue),        //    "cn1t>",     # -nc > -nt
    Rule("dd", Some("d"), false, stop),             //    "dd1.",      # -dd > -d
    Rule("ied", Some("y"), false, continue),        //    "dei3y>",    # -ied > -y
    Rule("ceed", Some("cess"), false, stop),        //    "deec2ss.",  # -ceed >", -cess
    Rule("eed", Some("ee"), false, stop),           //    "dee1.",     # -eed > -ee
    Rule("ed", Erase, false, continue),             //    "de2>",      # -ed > -
    Rule("hood", Erase, false, continue),           //    "dooh4>",    # -hood > -
    Rule("e", Erase, false, continue),              //    "e1>",       # -e > -
    Rule("lief", Some("liev"), false, stop),        //    "feil1v.",   # -lief > -liev
    Rule("if", Erase, false, continue),             //    "fi2>",      # -if > -
    Rule("ing", Erase, false, continue),            //    "gni3>",     # -ing > -
    Rule("iag", Some("y"), false, stop),            //    "gai3y.",    # -iag > -y
    Rule("ag", Erase, false, continue),             //    "ga2>",      # -ag > -
    Rule("gg", Some("g"), false, stop),             //    "gg1.",      # -gg > -g
    Rule("th", Erase, true, stop),                  //    "ht*2.",     # -th > -   if intact
    Rule("guish", Some("ct"), false, stop),         //    "hsiug5ct.", # -guish > -ct
    Rule("ish", Erase, false, continue),            //    "hsi3>",     # -ish > -
    Rule("i", Erase, true, stop),                   //    "i*1.",      # -i > -    if intact
    Rule("i", Some("y"), false, continue),          //    "i1y>",      # -i > -y
    Rule("ij", Some("id"), false, stop),            //    "ji1d.",     # -ij > -id   --  see nois4j> & vis3j>
    Rule("fuj", Some("fus"), false, stop),          //    "juf1s.",    # -fuj > -fus
    Rule("uj", Some("ud"), false, stop),            //    "ju1d.",     # -uj > -ud
    Rule("oj", Some("od"), false, stop),            //    "jo1d.",     # -oj > -od
    Rule("hej", Some("her"), false, stop),          //    "jeh1r.",    # -hej > -her
    Rule("verj", Some("vert"), false, stop),        //    "jrev1t.",   # -verj > -vert
    Rule("misj", Some("mit"), false, stop),         //    "jsim2t.",   # -misj > -mit
    Rule("nj", Some("nd"), false, stop),            //    "jn1d.",     # -nj > -nd
    Rule("j", Some("s"), false, stop),              //    "j1s.",      # -j > -s
    Rule("ifiabl", Erase, false, stop),             //    "lbaifi6.",  # -ifiabl > -
    Rule("iabl", Some("y"), false, stop),           //    "lbai4y.",   # -iabl > -y
    Rule("abl", Erase, false, continue),            //    "lba3>",     # -abl > -
    Rule("ibl", Erase, false, stop),                //    "lbi3.",     # -ibl > -
    Rule("bil", Some("bl"), false, continue),       //    "lib2l>",    # -bil > -bl
    Rule("cl", Some("c"), false, stop),             //    "lc1.",      # -cl > c
    Rule("iful", Some("y"), false, stop),           //    "lufi4y.",   # -iful > -y
    Rule("ful", Erase, false, continue),            //    "luf3>",     # -ful > -
    Rule("ul", Erase, false, stop),                 //    "lu2.",      # -ul > -
    Rule("ial", Erase, false, continue),            //    "lai3>",     # -ial > -
    Rule("ual", Erase, false, continue),            //    "lau3>",     # -ual > -
    Rule("al", Erase, false, continue),             //    "la2>",      # -al > -
    Rule("ll", Some("l"), false, stop),             //    "ll1.",      # -ll > -l
    Rule("ium", Erase, false, stop),                //    "mui3.",     # -ium > -
    Rule("um", Erase, true, stop),                  //    "mu*2.",     # -um > -   if intact
    Rule("ism", Erase, false, continue),            //    "msi3>",     # -ism > -
    Rule("mm", Some("m"), false, stop),             //    "mm1.",      # -mm > -m
    Rule("sion", Some("j"), false, continue),       //    "nois4j>",   # -sion > -j
    Rule("xion", Some("ct"), false, stop),          //    "noix4ct.",  # -xion > -ct
    Rule("ion", Erase, false, continue),            //    "noi3>",     # -ion > -
    Rule("ian", Erase, false, continue),            //    "nai3>",     # -ian > -
    Rule("an", Erase, false, continue),             //    "na2>",      # -an > -
    Rule("een", None, false, stop),                 //    "nee0.",     # protect  -een
    Rule("en", Erase, false, continue),             //    "ne2>",      # -en > -
    Rule("nn", Some("n"), false, stop),             //    "nn1.",      # -nn > -n
    Rule("ship", Erase, false, continue),           //    "pihs4>",    # -ship > -
    Rule("pp", Some("p"), false, stop),             //    "pp1.",      # -pp > -p
    Rule("er", Erase, false, continue),             //    "re2>",      # -er > -
    Rule("ear", None, false, stop),                 //    "rae0.",     # protect  -ear
    Rule("ar", Erase, false, stop),                 //    "ra2.",      # -ar > -
    Rule("or", Erase, false, continue),             //    "ro2>",      # -or > -
    Rule("ur", Erase, false, continue),             //    "ru2>",      # -ur > -
    Rule("rr", Some("r"), false, stop),             //    "rr1.",      # -rr > -r
    Rule("tr", Some("t"), false, continue),         //    "rt1>",      # -tr > -t
    Rule("ier", Some("y"), false, continue),        //    "rei3y>",    # -ier > -y
    Rule("ies", Some("y"), false, continue),        //    "sei3y>",    # -ies > -y
    Rule("sis", Some("s"), false, stop),            //    "sis2.",     # -sis > -s
    Rule("is", Erase, false, continue),             //    "si2>",      # -is > -
    Rule("ness", Erase, false, continue),           //    "ssen4>",    # -ness > -
    Rule("ss", None, false, stop),                  //    "ss0.",      # protect  -ss
    Rule("ous", Erase, false, continue),            //    "suo3>",     # -ous > -
    Rule("us", Erase, true, stop),                  //    "su*2.",     # -us > -   if intact
    Rule("s", Erase, true, continue),               //    "s*1>",      # -s > -    if intact
    Rule("s", None, false, stop),                   //    "s0.",       # protect -s
    Rule("plicat", Some("ply"), false, stop),       //    "tacilp4y.", # -plicat > -ply
    Rule("at", Erase, false, continue),             //    "ta2>",      # -at > -
    Rule("ment", Erase, false, continue),           //    "tnem4>",    # -ment > -
    Rule("ent", Erase, false, continue),            //    "tne3>",     # -ent > -
    Rule("ant", Erase, false, continue),            //    "tna3>",     # -ant > -
    Rule("ript", Some("rib"), false, stop),         //    "tpir2b.",   # -ript > -rib
    Rule("orpt", Some("orb"), false, stop),         //    "tpro2b.",   # -orpt > -orb
    Rule("duct", Some("duc"), false, stop),         //    "tcud1.",    # -duct > -duc
    Rule("sumpt", Some("sum"), false, stop),        //    "tpmus2.",   # -sumpt > -sum
    Rule("cept", Some("ceiv"), false, stop),        //    "tpec2iv.",  # -cept > -ceiv
    Rule("olut", Some("olv"), false, stop),         //    "tulo2v.",   # -olut > -olvn
    Rule("sist", None, false, stop),                //    "tsis0.",    # protect  -sist
    Rule("ist", Erase, false, continue),            //    "tsi3>",     # -ist > -
    Rule("tt", Some("t"), false, stop),             //    "tt1.",      # -tt > -t
    Rule("iqu", Erase, false, stop),                //    "uqi3.",     # -iqu > -
    Rule("ogu", Some("og"), false, stop),           //    "ugo1.",     # -ogu > -og
    Rule("siv", Some("j"), false, continue),        //    "vis3j>",    # -siv > -j
    Rule("eiv", None, false, stop),                 //    "vie0.",     # protect  -eiv
    Rule("iv", Erase, false, continue),             //    "vi2>",      # -iv > -
    Rule("bly", Some("bl"), false, continue),       //    "ylb1>",     # -bly > -bl
    Rule("ily", Some("y"), false, continue),        //    "yli3y>",    # -ily > -y
    Rule("ply", None, false, stop),                 //    "ylp0.",     # protect  -ply
    Rule("ly", Erase, false, continue),             //    "yl2>",      # -ly > -
    Rule("ogy", Some("og"), false, stop),           //    "ygo1.",     # -ogy > -og
    Rule("phy", Some("ph"), false, stop),           //    "yhp1.",     # -phy > -ph
    Rule("omg", Some("om"), false, stop),           //    "ymo1.",     # -omy > -om
    Rule("opy", Some("op"), false, stop),           //    "ypo1.",     # -opy > -op
    Rule("ity", Erase, false, continue),            //    "yti3>",     # -ity > -
    Rule("ety", Erase, false, continue),            //    "yte3>",     # -ety > -
    Rule("lty", Some("l"), false, stop),            //    "ytl2.",     # -lty > -l
    Rule("istry", Erase, false, stop),              //    "yrtsi5.",   # -istry > -
    Rule("ary", Erase, false, continue),            //    "yra3>",     # -ary > -
    Rule("ory", Erase, false, continue),            //    "yro3>",     # -ory > -
    Rule("igy", Erase, false, stop),                //    "yfi3.",     # -ify > -
    Rule("ncy", Some("nt"), false, continue),       //    "ycn2t>",    # -ncy > -nt
    Rule("acy", Erase, false, continue),            //    "yca3>",     # -acy > -
    Rule("iz", Erase, false, continue),             //    "zi2>",      # -iz > -
    Rule("yz", Some("ys"), false, stop)             //    "zy1s."      # -yz > -ys
  )
}
