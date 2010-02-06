package hosc

import hosc.exp.{HigherLevelSuperCompilerApp => HLSC}

object HigherLevelExamples {
  def main(args : Array[String]) : Unit = {
    HLSC.main(Array("-si", "hl/or_even_odd.hs", "-t", "hl/out/or_even_odd.svg","-p", "hl/out/or_even_odd.hs"))
    HLSC.main(Array("-si", "hl/even_doubleAcc.hs", "-t", "hl/out/even_doubleAcc.svg","-p", "hl/out/even_doubleAcc.hs"))
    
    //HLSC.main(Array("-si", "hl/app_rev.hs", "-t", "hl/out/app_rev.svg","-p", "hl/out/app_rev.hs"))
  }
}
