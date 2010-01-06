package hosc

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import HLanguage._
import Util._

import hosc.{SuperCompilerApp => SCP0}
import hosc.exp.{SuperCompilerApp1 => SCP1}

class Experiments {
  // synapse with choice
  @Test def synapse =
    SCP0.main(Array("-si", "exp/synapse.hs", "-t", "exp/out0/synapse.svg","-p", "exp/out0/synapse.hs"))
  
  @Test def nondet1 =
    SCP0.main(Array("-si", "exp/nondet1.hs", "-t", "exp/out0/nondet1.svg","-p", "exp/out0/nondet1.hs"))
  
  @Test def nondet2 =
    SCP0.main(Array("-si", "exp/nondet2.hs", "-t", "exp/out0/nondet2.svg","-p", "exp/out0/nondet2.hs"))
  
 @Ignore 
 @Test def eval1_0 =
    SCP0.main(Array("-si", "exp/eval1.hs", "-t", "exp/out0/eval1.svg","-p", "exp/out0/eval1.hs"))
 
 @Ignore 
 @Test def eval1_1: Unit = {
    //SCP1.main(Array("-si", "exp/eval1.hs", "-t", "exp/out1/eval1.svg","-p", "exp/out1/eval1.hs"))
    null
  }  
  
  @Test def fo_dsl_1_a =
    SCP0.main(Array("-si", "exp/fo_dsl_1_a.hs", "-t", "exp/out0/fo_dsl_1_a.svg","-p", "exp/out0/fo_dsl_1_a.hs"))
  
  @Test def fo_dsl_1_b =
    SCP0.main(Array("-si", "exp/fo_dsl_1_b.hs", "-t", "exp/out0/fo_dsl_1_b.svg","-p", "exp/out0/fo_dsl_1_b.hs"))
  
  @Test def fo_dsl_2_a =
    SCP0.main(Array("-si", "exp/fo_dsl_2_a.hs", "-t", "exp/out0/fo_dsl_2_a.svg","-p", "exp/out0/fo_dsl_2_a.hs"))
  
  @Test def ho_dsl =
    SCP0.main(Array("-si", "exp/ho_dsl.hs", "-t", "exp/out0/ho_dsl.svg","-p", "exp/out0/ho_dsl.hs"))
  
  @Ignore
  @Test def fo_dsl_2_b = {
    SCP0.main(Array("-si", "exp/fo_dsl_2_b.hs", "-t", "exp/out0/fo_dsl_2_b.svg","-p", "exp/out0/fo_dsl_2_b.hs"))
  }
  
  @Test def fo_dsl_3_a =
    SCP0.main(Array("-si", "exp/fo_dsl_3_a.hs", "-t", "exp/out0/fo_dsl_3_a.svg","-p", "exp/out0/fo_dsl_3_a.hs"))
  
  @Test def fo_dsl_3_b =
    SCP0.main(Array("-si", "exp/fo_dsl_3_b.hs", "-t", "exp/out0/fo_dsl_3_b.svg","-p", "exp/out0/fo_dsl_3_b.hs"))
}
