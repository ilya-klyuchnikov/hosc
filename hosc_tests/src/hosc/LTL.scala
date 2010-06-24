package hosc

import hosc.{SuperCompilerApp => SCP0, SCLemApp}
import hosc.exp.{SuperCompilerApp1 => SCP1, SuperCompilerApp2 => SCP2}

object LTL {
	def main(args : Array[String]) : Unit = {
	   SCLemApp.main(Array("-si", "mc/LTL1.hs", "-li", "mc/lemmas.hs", "-t", "sc/out0/LTL1.svg", "-p", "sc/out0/LTL1.hs"))
	}
}
