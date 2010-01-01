package hosc


import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import HLanguage._
import Util._

import hosc.{SuperCompilerApp => SCP0}
import hosc.exp.{SuperCompilerApp1 => SCP1}

class Experiments {
 @Test def eval1_0 = {
    SCP0.main(Array("-si", "exp/eval1.hs", "-t", "exp/out0/eval1.svg","-p", "exp/out0/eval1.hs"))
  }
 
 @Test def eval1_1 = {
    SCP1.main(Array("-si", "exp/eval1.hs", "-t", "exp/out1/eval1.svg","-p", "exp/out1/eval1.hs"))
  }
}
