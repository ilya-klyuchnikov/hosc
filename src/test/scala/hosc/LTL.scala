package hosc

import hosc.exp.{SuperCompilerApp1 => SCP1, SuperCompilerApp2 => SCP2}

object LTL {
	def main(args : Array[String]) : Unit = {
	   SCLemApp.main(Array("-si", "examples/mc/LTL1.hs", "-li", "examples/mc/lemmas.hs", "-t", "out0/LTL1.svg", "-p", "out0/LTL1.hs"))
	}
}
