package hosc

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import HLanguage._
import Util._

import hosc.{SuperCompilerApp => SCP}

class SuperCompilerTest {
  
  @Test def t01_loop = 
    SCP.main(Array("-si", "sc/01_loop.hs", "-t", "sc/out0/01_loop.svg","-p", "sc/out0/01_loop.hs"))
  
  // producing bottom: TODO -> make special case for program generator 
  @Test def loop = 
    SCP.main(Array("-si", "sc/loop.hs", "-t", "sc/out0/loop.svg", "-p", "sc/out0/loop.hs"))
  
  @Test def eqnum_plus = 
    SCP.main(Array("-si", "sc/eqnum_plus.hs", "-t", "sc/out0/eqnum_plus.svg", "-p", "sc/out0/eqnum_plus.hs"))
  
  @Test def eq = 
    SCP.main(Array("-si", "sc/eq.hs", "-t", "sc/out0/eq.svg", "-p", "sc/out0/eq.hs"))
  
  @Test def zip1 = 
    SCP.main(Array("-si", "sc/zip1.hs", "-t", "sc/out0/zip1.svg", "-p", "sc/out0/zip1.hs"))
    
  @Test def zip2 = 
    SCP.main(Array("-si", "sc/zip2.hs", "-t", "sc/out0/zip2.svg", "-p", "sc/out0/zip2.hs"))
  
  @Test def regexp(): Unit = 
    SCP.main(Array("-si", "sc/regexp.hs", "-t", "sc/out0/regexp.svg", "-p", "sc/out0/regexp.hs"))
  
  @Test def synapse() = 
    SCP.main(Array("-si", "sc/synapse.hs", "-t", "sc/out0/synapse.svg", "-p", "sc/out0/synapse.hs"))
  
  @Test def synapse1() = 
    SCP.main(Array("-si", "sc/synapse1.hs",  "-t", "sc/out0/synapse1.svg", "-p", "sc/out0/synapse1.hs"))
  
  @Test def synapse2() = 
    SCP.main(Array("-si", "sc/synapse2.hs", "-t", "sc/out0/synapse2.svg", "-p", "sc/out0/synapse2.hs"))
  
  @Test def even() = 
    SCP.main(Array("-si", "sc/even.hs", "-t", "sc/out0/even.svg", "-p", "sc/out0/even.hs"))
    
  @Test def even1() = 
    SCP.main(Array("-si", "sc/even1.hs", "-t", "sc/out0/even1.svg", "-p", "sc/out0/even1.hs"))
  
  @Test def even2() = 
    SCP.main(Array("-si", "sc/even2.hs", "-t", "sc/out0/even2.svg", "-p", "sc/out0/even2.hs"))
  
  @Test def evenSS() = 
    SCP.main(Array("-si", "sc/evenSS.hs", "-t", "sc/out0/evenSS.svg", "-p", "sc/out0/evenSS.hs"))
  
  // -----
  @Test def case_() = 
    SCP.main(Array("-si", "sc/case.hs", "-t", "sc/out0/case.svg", "-p", "sc/out0/case.hs"))
  
  @Test def eqnum() = 
    SCP.main(Array("-si", "sc/eqnum.hs", "-t", "sc/out0/eqnum.svg", "-p", "sc/out0/eqnum.hs"))
  
  @Test def eqnumxx() = 
    SCP.main(Array("-si", "sc/eqnumxx.hs", "-t", "sc/out0/eqnumxx.svg", "-p", "sc/out0/eqnumxx.hs"))
  
  @Test def exp() = 
    SCP.main(Array("-si", "sc/exp.hs", "-t", "sc/out0/exp.svg", "-p", "sc/out0/exp.hs"))
  
  @Test def fib() = 
    SCP.main(Array("-si", "sc/fib.hs", "-t", "sc/out0/fib.svg", "-p", "sc/out0/fib.hs"))
 
  @Test def filter() = 
    SCP.main(Array("-si", "sc/filter.hs", "-t", "sc/out0/filter.svg", "-p", "sc/out0/filter.hs"))
  
  @Test def isort() = 
    SCP.main(Array("-si", "sc/isort.hs", "-t", "sc/out0/isort.svg", "-p", "sc/out0/isort.hs"))
  
  @Test def leq_add() = 
    SCP.main(Array("-si", "sc/leq_add.hs", "-t", "sc/out0/leq_add.svg", "-p", "sc/out0/leq_add.hs"))
  
  @Test def letrec_rev() = 
    SCP.main(Array("-si", "sc/letrec_rev.hs", "-t", "sc/out0/letrec_rev.svg", "-p", "sc/out0/letrec_rev.hs"))
  
  @Test def letrec() = 
    SCP.main(Array("-si", "sc/letrec.hs", "-t", "sc/out0/letrec.svg", "-p", "sc/out0/letrec.hs"))  
  
  @Test def min() = 
    SCP.main(Array("-si", "sc/min.hs", "-t", "sc/out0/min.svg", "-p", "sc/out0/min.hs"))
  
  @Test def takenm() = 
    SCP.main(Array("-si", "sc/takenm.hs", "-t", "sc/out0/takenm.svg", "-p", "sc/out0/takenm.hs"))
  
  @Test def war() = 
    SCP.main(Array("-si", "sc/var.hs", "-t", "sc/out0/var.svg", "-p", "sc/out0/var.hs"))
  
   @Test def church() = 
    SCP.main(Array("-si", "sc/church.hs", "-t", "sc/out0/church.svg", "-p", "sc/out0/church.hs"))
   
   @Test def fix() = 
    SCP.main(Array("-si", "sc/fix.hs", "-t", "sc/out0/fix.svg", "-p", "sc/out0/fix.hs"))
   
   @Test def drec() = 
    SCP.main(Array("-si", "sc/drec.hs", "-t", "sc/out0/drec.svg", "-p", "sc/out0/drec.hs"))
   
   @Test def enc() = 
    SCP.main(Array("-si", "sc/encoding.hs", "-t", "sc/out0/enc.svg", "-p", "sc/out0/enc.hs"))
   
   @Test def sf() = 
    SCP.main(Array("-si", "sc/sf.hs", "-t", "sc/out0/sf.svg", "-p", "sc/out0/sf.hs"))
   
   @Test def rev() = 
    SCP.main(Array("-si", "sc/rev.hs", "-t", "sc/out0/rev.svg", "-p", "sc/out0/rev.hs"))
   
   @Test def synapse0() = 
    SCP.main(Array("-si", "sc/synapse0.hs",  "-t", "sc/out0/synapse0.svg", "-p", "sc/out0/synapse0.hs"))
}
