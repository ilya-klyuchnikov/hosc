package hosc

import hosc.exp.{HigherLevelSuperCompilerApp => HLSC}
import hosc.exp.{FullPositiveSuperCompilerApp => FPSC}

object HigherLevelExamples {
  def main(args : Array[String]) : Unit = {
    HLSC.main(Array("-si", "hl/or_even_odd.hs", "-t", "hl/out/or_even_odd.svg","-p", "hl/out/or_even_odd.hs"))
    HLSC.main(Array("-si", "hl/and_even_odd.hs", "-t", "hl/out/and_even_odd.svg","-p", "hl/out/and_even_odd.hs"))
    HLSC.main(Array("-si", "hl/even_plus_xx.hs", "-t", "hl/out/even_plus_xx.svg","-p", "hl/out/even_plus_xx.hs"))
    HLSC.main(Array("-si", "hl/add_x_x.hs", "-t", "hl/out/add_x_x.svg","-p", "hl/out/add_x_x.hs"))
    HLSC.main(Array("-si", "hl/even_doubleAcc.hs", "-t", "hl/out/even_doubleAcc.svg","-p", "hl/out/even_doubleAcc.hs"))
    //HLSC.main(Array("-si", "hl/eqnum.hs", "-t", "hl/out/eqnum.svg","-p", "hl/out/eqnum.hs"))
    
    //HLSC.main(Array("-si", "hl/app_rev.hs", "-t", "hl/out/app_rev.svg","-p", "hl/out/app_rev.hs"))
    //FPSC.main(Array("-si", "hl/church.hs", "-t", "hl/out/church.svg","-p", "hl/out/church.hs"))
  }
}