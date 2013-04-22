package hosc.debug

import hosc.{SuperCompilerApp => SCP, SCLemApp}
import hosc.exp.{SuperCompilerApp1 => SCP1}

object Debug {

  def main(args: Array[String]): Unit = {
    //SCP.main(Array("-si", "debug/d1.hs", "-t", "debug/d1.svg","-p", "debug/d1.out.hs"))
    //SCP.main(Array("-si", "debug/m1.hs", "-t", "debug/m1.svg","-p", "debug/m1.out.hs"))
    //SCP.main(Array("-si", "debug/m2.hs", "-t", "debug/m2.svg","-p", "debug/m2.out.hs"))
    //SCP.main(Array("-si", "debug/tree.in.hs", "-t", "debug/tree.svg","-p", "debug/tree.out.hs"))
    //SCLemApp.main(Array("-si", "debug/d1.hs", "-li", "debug/lemmas.hs", "-t", "debug/out1.svg","-p", "debug/out1.hs"))
    //SCP.main(Array("-si", "debug/krivine1.hs", "-t", "debug/krivine1.svg","-p", "debug/krivine1.out.hs"))
	//SCP.main(Array("-si", "debug/explode2.hs", "-t", "debug/explode2.svg","-p", "debug/explode2.out.hs"))
    //SCP.main(Array("-si", "debug/explode3.hs", "-t", "debug/explode3.svg","-p", "debug/explode3.out.hs"))
    //SCP.main(Array("-si", "debug/explode4.hs", "-t", "debug/explode4.svg","-p", "debug/explode4.out.hs"))
	  SCP.main(Array("-si", "examples/debug/add_machine.hs", "-t", "out/debug/am.svg","-p", "out/debug/am.out.hs"))
  }
}
