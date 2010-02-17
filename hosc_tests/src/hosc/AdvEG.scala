package hosc

import Util._
import HLanguage._
import LangUtils._
import hosc.lemmas.AdvancedExpressionGenerator

object AdvEG {
  def main(args : Array[String]) : Unit = {
    val p = programFromFile("hl/rev3.hs")
    val ags = AdvancedExpressionGenerator.generate(p.goal)
    for (ag <- ags) {
      println(ag)
      println()
    }
  }
}
