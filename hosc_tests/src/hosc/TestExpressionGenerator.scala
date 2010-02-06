package hosc

import Util._
import HLanguage._
import LangUtils._
import hosc.lemmas.ExpressionGenerator

object TestExpressionGenerator {
  def main(args : Array[String]) : Unit = {
    val start = System.currentTimeMillis
    val p = programFromFile("sc/list_funcs.hs")
    val gen = new ExpressionGenerator(p)
    val vars = List("x", "y", "z") map {Variable(_)}
    val exps = gen.generateAll(3, vars)
    var i = 0
    for (exp <- exps) {
      i += 1
      print(i + ": ")
      println(exp)
    }
    val end = System.currentTimeMillis
    val time = end - start
    println(i)
    Console.println("Took " + (end-start)/1000.0 + "s")
  }
}
