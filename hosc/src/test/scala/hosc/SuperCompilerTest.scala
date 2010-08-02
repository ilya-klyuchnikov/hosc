package hosc

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import HLanguage._
import Util._

import hosc.{SuperCompilerApp => SCP}

class SuperCompilerTest {
  
  @Test def t01_loop = 
    SCP.main(Array("-si", "examples/sc/01_loop.hs", "-t", "examples/sc/out0/01_loop.svg","-p", "examples/sc/out0/01_loop.hs"))
  
  // producing bottom: TODO -> make special case for program generator 
  @Test def loop = 
    SCP.main(Array("-si", "examples/sc/loop.hs", "-t", "examples/sc/out0/loop.svg", "-p", "examples/sc/out0/loop.hs"))
  
  @Test def eqnum_plus = 
    SCP.main(Array("-si", "examples/sc/eqnum_plus.hs", "-t", "examples/sc/out0/eqnum_plus.svg", "-p", "examples/sc/out0/eqnum_plus.hs"))
  
  @Test def eq = 
    SCP.main(Array("-si", "examples/sc/eq.hs", "-t", "examples/sc/out0/eq.svg", "-p", "examples/sc/out0/eq.hs"))
  
  @Test def zip1 = 
    SCP.main(Array("-si", "examples/sc/zip1.hs", "-t", "examples/sc/out0/zip1.svg", "-p", "examples/sc/out0/zip1.hs"))
    
  @Test def zip2 = 
    SCP.main(Array("-si", "examples/sc/zip2.hs", "-t", "examples/sc/out0/zip2.svg", "-p", "examples/sc/out0/zip2.hs"))
  
  @Test def regexp(): Unit = 
    SCP.main(Array("-si", "examples/sc/regexp.hs", "-t", "examples/sc/out0/regexp.svg", "-p", "examples/sc/out0/regexp.hs"))
  
  @Test def synapse() = 
    SCP.main(Array("-si", "examples/sc/synapse.hs", "-t", "examples/sc/out0/synapse.svg", "-p", "examples/sc/out0/synapse.hs"))
  
  @Test def synapse1() = 
    SCP.main(Array("-si", "examples/sc/synapse1.hs",  "-t", "examples/sc/out0/synapse1.svg", "-p", "examples/sc/out0/synapse1.hs"))
  
  @Test def synapse2() = 
    SCP.main(Array("-si", "examples/sc/synapse2.hs", "-t", "examples/sc/out0/synapse2.svg", "-p", "examples/sc/out0/synapse2.hs"))
  
  @Test def even() = 
    SCP.main(Array("-si", "examples/sc/even.hs", "-t", "examples/sc/out0/even.svg", "-p", "examples/sc/out0/even.hs"))
    
  @Test def even1() = 
    SCP.main(Array("-si", "examples/sc/even1.hs", "-t", "examples/sc/out0/even1.svg", "-p", "examples/sc/out0/even1.hs"))
  
  @Test def even2() = 
    SCP.main(Array("-si", "examples/sc/even2.hs", "-t", "examples/sc/out0/even2.svg", "-p", "examples/sc/out0/even2.hs"))
  
  @Test def evenSS() = 
    SCP.main(Array("-si", "examples/sc/evenSS.hs", "-t", "examples/sc/out0/evenSS.svg", "-p", "examples/sc/out0/evenSS.hs"))
  
  // -----
  @Test def case_() = 
    SCP.main(Array("-si", "examples/sc/case.hs", "-t", "examples/sc/out0/case.svg", "-p", "examples/sc/out0/case.hs"))
  
  @Test def eqnum() = 
    SCP.main(Array("-si", "examples/sc/eqnum.hs", "-t", "examples/sc/out0/eqnum.svg", "-p", "examples/sc/out0/eqnum.hs"))
  
  @Test def eqnumxx() = 
    SCP.main(Array("-si", "examples/sc/eqnumxx.hs", "-t", "examples/sc/out0/eqnumxx.svg", "-p", "examples/sc/out0/eqnumxx.hs"))
  
  @Test def exp() = 
    SCP.main(Array("-si", "examples/sc/exp.hs", "-t", "examples/sc/out0/exp.svg", "-p", "examples/sc/out0/exp.hs"))
  
  @Test def fib() = 
    SCP.main(Array("-si", "examples/sc/fib.hs", "-t", "examples/sc/out0/fib.svg", "-p", "examples/sc/out0/fib.hs"))
 
  @Test def filter() = 
    SCP.main(Array("-si", "examples/sc/filter.hs", "-t", "examples/sc/out0/filter.svg", "-p", "examples/sc/out0/filter.hs"))
  
  @Test def isort() = 
    SCP.main(Array("-si", "examples/sc/isort.hs", "-t", "examples/sc/out0/isort.svg", "-p", "examples/sc/out0/isort.hs"))
  
  @Test def leq_add() = 
    SCP.main(Array("-si", "examples/sc/leq_add.hs", "-t", "examples/sc/out0/leq_add.svg", "-p", "examples/sc/out0/leq_add.hs"))
  
  @Test def letrec_rev() = 
    SCP.main(Array("-si", "examples/sc/letrec_rev.hs", "-t", "examples/sc/out0/letrec_rev.svg", "-p", "examples/sc/out0/letrec_rev.hs"))
  
  @Test def letrec() = 
    SCP.main(Array("-si", "examples/sc/letrec.hs", "-t", "examples/sc/out0/letrec.svg", "-p", "examples/sc/out0/letrec.hs"))  
  
  @Test def min() = 
    SCP.main(Array("-si", "examples/sc/min.hs", "-t", "examples/sc/out0/min.svg", "-p", "examples/sc/out0/min.hs"))
  
  @Test def takenm() = 
    SCP.main(Array("-si", "examples/sc/takenm.hs", "-t", "examples/sc/out0/takenm.svg", "-p", "examples/sc/out0/takenm.hs"))
  
  @Test def war() = 
    SCP.main(Array("-si", "examples/sc/var.hs", "-t", "examples/sc/out0/var.svg", "-p", "examples/sc/out0/var.hs"))
  
   @Test def church() = 
    SCP.main(Array("-si", "examples/sc/church.hs", "-t", "examples/sc/out0/church.svg", "-p", "examples/sc/out0/church.hs"))
   
   @Test def fix() = 
    SCP.main(Array("-si", "examples/sc/fix.hs", "-t", "examples/sc/out0/fix.svg", "-p", "examples/sc/out0/fix.hs"))
   
   @Test def drec() = 
    SCP.main(Array("-si", "examples/sc/drec.hs", "-t", "examples/sc/out0/drec.svg", "-p", "examples/sc/out0/drec.hs"))
   
   @Test def enc() = 
    SCP.main(Array("-si", "examples/sc/encoding.hs", "-t", "examples/sc/out0/enc.svg", "-p", "examples/sc/out0/enc.hs"))
   
   @Test def sf() = 
    SCP.main(Array("-si", "examples/sc/sf.hs", "-t", "examples/sc/out0/sf.svg", "-p", "examples/sc/out0/sf.hs"))
   
   @Test def rev() = 
    SCP.main(Array("-si", "examples/sc/rev.hs", "-t", "examples/sc/out0/rev.svg", "-p", "examples/sc/out0/rev.hs"))
   
   @Test def synapse0() = 
    SCP.main(Array("-si", "examples/sc/synapse0.hs",  "-t", "examples/sc/out0/synapse0.svg", "-p", "examples/sc/out0/synapse0.hs"))
   
   @Test def fix_con = 
    SCP.main(Array("-si", "examples/sc/fix_con.hs", "-t", "examples/sc/out0/fix_con.svg", "-p", "examples/sc/out0/fix_con.hs"))
   
   @Test def ltl1 = 
    SCP.main(Array("-si", "examples/mc/ltl1.hs", "-t", "examples/sc/out0/ltl1.svg", "-p", "examples/sc/out0/ltl1.hs"))
   
   @Ignore
   @Test def ltl2 = 
    SCP.main(Array("-si", "examples/mc/ltl2.hs", "-t", "examples/sc/out0/ltl2.svg", "-p", "examples/sc/out0/ltl2.hs"))
   
   @Test def ltl4 = 
    SCP.main(Array("-si", "examples/mc/ltl4.hs", "-t", "examples/sc/out0/ltl4.svg", "-p", "examples/sc/out0/ltl4.hs"))
   
   @Test def ltl5 = 
    SCP.main(Array("-si", "examples/mc/ltl5.hs", "-t", "examples/sc/out0/ltl5.svg", "-p", "examples/sc/out0/ltl5.hs"))
   
   @Test def iterate = 
    SCP.main(Array("-si", "examples/sc/iterate.hs", "-t", "examples/sc/out0/iterate.svg", "-p", "examples/sc/out0/iterate.hs"))
   
   @Test def fixDouble = 
    SCP.main(Array("-si", "examples/sc/fixDouble.hs", "-t", "examples/sc/out0/fixDouble.svg", "-p", "examples/sc/out0/fixDouble.hs"))
   
    @Test def iter2 = 
    SCP.main(Array("-si", "examples/sc/iter2.hs", "-t", "examples/sc/out0/iter2.svg", "-p", "examples/sc/out0/iter2.hs"))
    
  @Test def russel_bad = 
    SCP.main(Array("-si", "examples/sc/russel_bad.hs", "-t", "examples/sc/out0/russel_bad.svg", "-p", "examples/sc/out0/russel_bad.hs"))
}
