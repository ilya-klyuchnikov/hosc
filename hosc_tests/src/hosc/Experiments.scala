package hosc

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import HLanguage._
import Util._

import hosc.{SuperCompilerApp => SCP0}
import hosc.exp.{SuperCompilerApp1 => SCP1}

class Experiments {
 @Ignore 
 @Test def eval1_0 =
    SCP0.main(Array("-si", "exp/eval1.hs", "-t", "exp/out0/eval1.svg","-p", "exp/out0/eval1.hs"))
 
 @Ignore 
 @Test def eval1_1 =
    SCP1.main(Array("-si", "exp/eval1.hs", "-t", "exp/out1/eval1.svg","-p", "exp/out1/eval1.hs"))
 
  @Ignore
  @Test def synapse0 =
    SCP1.main(Array("-si", "exp/synapse0.hs", "-t", "exp/out0/synapse1.svg","-p", "exp/out0/synapse1.hs"))
  
  @Test def fo_dsl_1_a =
    SCP0.main(Array("-si", "exp/fo_dsl_1_a.hs", "-t", "exp/out0/fo_dsl_1_a.svg","-p", "exp/out0/fo_dsl_1_a.hs"))
  
  @Test def fo_dsl_1_b =
    SCP0.main(Array("-si", "exp/fo_dsl_1_b.hs", "-t", "exp/out0/fo_dsl_1_b.svg","-p", "exp/out0/fo_dsl_1_b.hs"))
  @Ignore
  @Test def fo_dsl_2_a =
    SCP1.main(Array("-si", "exp/fo_dsl_2_a.hs", "-t", "exp/out0/fo_dsl_2_a.svg","-p", "exp/out0/fo_dsl_2_a.hs"))
  @Ignore
  @Test def fo_dsl_2_b = {
    SCP0.main(Array("-si", "exp/fo_dsl_2_b.hs", "-t", "exp/out0/fo_dsl_2_b.svg","-p", "exp/out0/fo_dsl_2_b.hs"))
  }
}
