package hosc

import hosc.exp.{HigherLevelSuperCompilerApp => HLSC}
import hosc.exp.{FullPositiveSuperCompilerApp => FPSC}

object HigherLevelExamples {
  def main(args : Array[String]) : Unit = {
    // 6 sec
    HLSC.main(Array("-si", "examples/hl/or_even_odd.hs", "-t", "out/hl/or_even_odd.svg","-p", "out/hl/or_even_odd.hs"))
    // 92 sec
    //HLSC.main(Array("-si", "examples/hl/even_doubleAcc.hs", "-t", "out/hl/even_doubleAcc.svg","-p", "out/hl/even_doubleAcc.hs"))
    // 8 sec
    HLSC.main(Array("-si", "examples/hl/and_even_odd.hs", "-t", "out/hl/and_even_odd.svg","-p", "out/hl/and_even_odd.hs"))
    // 4 sec
    HLSC.main(Array("-si", "examples/hl/even_plus_xx.hs", "-t", "out/hl/even_plus_xx.svg","-p", "out/hl/even_plus_xx.hs"))
    // 5 sec
    HLSC.main(Array("-si", "examples/hl/add_x_x.hs", "-t", "out/hl/add_x_x.svg","-p", "out/hl/add_x_x.hs"))
    // 142 sec
    //HLSC.main(Array("-si", "examples/hl/eqnum.hs", "-t", "out/hl/eqnum.svg","-p", "out/hl/eqnum.hs"))

    // very slow - do not run
    //HLSC.main(Array("-si", "examples/hl/app_rev.hs", "-t", "out/hl/app_rev.svg","-p", "out/hl/app_rev.hs"))

    // fast
    FPSC.main(Array("-si", "examples/hl/church.hs", "-t", "out/hl/church.svg","-p", "out/hl/church.hs"))

    // very slow
    //HLSC.main(Array("-si", "examples/hl/tree_flatten.hs", "-t", "out/hl/tree_flatten.svg", "-p", "out/hl/tree_flatten.hs"))

    // very slow
    //HLSC.main(Array("-si", "examples/hl/eq_tree_leaves.hs", "-t", "out/hl/eq_tree_leaves.svg","-p", "out/hl/eq_tree_leaves.hs"))
    // very slow
    //HLSC.main(Array("-si", "examples/hl/eq_tree.hs", "-t", "out/hl/eq_tree.svg","-p", "out/hl/eq_tree.hs"))
    // fast
    HLSC.main(Array("-si", "examples/hl/rev_map.hs", "-t", "out/hl/rev_map.svg","-p", "out/hl/rev_map.hs"))

  }
}
