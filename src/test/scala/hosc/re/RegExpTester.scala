package hosc.re

import hosc.TUtils
import hosc.HLanguage._
import hosc.LangUtils._
import hosc.SuperCompiler0
import hosc.CodeConstructor
import hosc.Eq

object RegExpTester {
  lazy val program = TUtils.programResultFromFile("examples/re/regexp2.hs").get
 
  def superCompile(goal: Expression) = {
    val sc = new SuperCompiler0(program)
    val pt = sc.buildProcessTree(goal)
    val g = new CodeConstructor(program, pt, true)
    val result = g.generateProgram()
    result
  }
  
  def generateWord(prefixSize: Int, finite: Boolean): String = {
    val core = if (finite) {"Nil"} else {"v"+ (prefixSize + 1)}
    val prefix = (1 until (prefixSize + 1)) map {"(Cons v" + _} mkString (" ")
    val result = prefix + " " + core + (0 until prefixSize).map{_ => ")"}.mkString("") 
    result
  }
  
  def testEq(re1: String, re2: String, wordPrefixSize: Int, finite: Boolean): Boolean = {
    val word = generateWord(wordPrefixSize, finite)
    //println(word)
    val goal1Str = "match " + re1 + " " + word
    val goal2Str = "match " + re2 + " " + word
    
    println(goal1Str)
    println(goal2Str)
    
    val goal1 = TUtils.termFromString(goal1Str, program)
    val goal2 = TUtils.termFromString(goal2Str, program)
    
    val sced1 = superCompile(goal1)
    val sced2 = superCompile(goal2)
    
    if (finite) {
      println(format(sced1.goal))
      println(format(sced2.goal))
    }
    
    val result = Eq.equivalent(sced1.goal, sced2.goal)
    result
  }
  
  def checkRegexps(re1: String, re2: String): Boolean = {
    println("----")
    println(re1 + " ? " + re2)
    var stop = false
    var prefixSize = 0
    var eq = false
    while (!stop) {
      println("\ttrying " + prefixSize)
      val eqInf = false //testEq(re1, re2, prefixSize, false)
      val eqFin = testEq(re1, re2, prefixSize, true)
      println("\t\t" + eqFin + " " + eqInf)
      prefixSize = prefixSize + 1
      eq = eqFin && eqInf
      if (eq || !eqFin) {
        stop = true
      }
      if (prefixSize == 4) {
        stop = true
      }
    }
    println(eq)
    eq
  }

  // ~ 1min
  def main(args: Array[String]): Unit = {
    //println(testEq("a", "b", 0, false))
    checkRegexps("(rep a)", "(rep (rep a))")


    checkRegexps("(or nil (rep a))", "(rep a)")


    // 6 (a*b)*a* == a*(ba*)*
    checkRegexps("(concat (rep (concat (rep a) b)) (rep a))", 
                 "(concat (rep a) (rep (concat b (rep a))))")

    // 7 (a+b)* == (a*b*)*
    checkRegexps("(rep (or a b))", 
                 "(rep (concat (rep a) (rep b)))")


    // slow
    // 9 (a|b) (a*b*)* = (a*b*)*(a|b)
    //checkRegexps("(concat (or a b) (rep (concat (rep a) (rep b))))",
    //             "(concat (rep (concat (rep a) (rep b))) (or a b))")

  }
}
