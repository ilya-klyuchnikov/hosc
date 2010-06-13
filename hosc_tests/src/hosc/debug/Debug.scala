package hosc.debug

import hosc.{SuperCompilerApp => SCP, SCLemApp}
import hosc.exp.{SuperCompilerApp1 => SCP1}

object Debug {

  def main(args: Array[String]): Unit = {
    SCP.main(Array("-si", "debug/add_machine.hs", "-t", "debug/a.svg","-p", "debug/a.hs"))
  }
}
