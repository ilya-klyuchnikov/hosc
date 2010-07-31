package hosc

import org.junit.Test
import hosc.exp.{SuperCompilerApp1 => SCP1}

class SuperCompiler1Test {
  @Test def t01_loop = 
    SCP1.main(Array("-si", "sc/01_loop.hs", "-t", "sc/out1/01_loop.svg","-p", "sc/out1/01_loop.hs"))
  
  @Test def loop = 
    SCP1.main(Array("-si", "sc/loop.hs", "-t", "sc/out1/loop.svg", "-p", "sc/out1/loop.hs"))
  
  @Test def eqnum_plus = 
    SCP1.main(Array("-si", "sc/eqnum_plus.hs", "-t", "sc/out1/eqnum_plus.svg", "-p", "sc/out1/eqnum_plus.hs"))
  
  @Test def eq = 
    SCP1.main(Array("-si", "sc/eq.hs", "-t", "sc/out1/eq.svg", "-p", "sc/out1/eq.hs"))
  
  @Test def zip1 = 
    SCP1.main(Array("-si", "sc/zip1.hs", "-t", "sc/out1/zip1.svg", "-p", "sc/out1/zip1.hs"))
    
  @Test def zip2 = 
    SCP1.main(Array("-si", "sc/zip2.hs", "-t", "sc/out1/zip2.svg", "-p", "sc/out1/zip2.hs"))
  
  @Test def regexp(): Unit = 
    SCP1.main(Array("-si", "sc/regexp.hs", "-t", "sc/out1/regexp.svg", "-p", "sc/out1/regexp.hs"))

  @Test def synapse() = 
    SCP1.main(Array("-si", "sc/synapse.hs", "-t", "sc/out1/synapse.svg", "-p", "sc/out1/synapse.hs"))
  
  @Test def synapse1() = 
    SCP1.main(Array("-si", "sc/synapse1.hs",  "-t", "sc/out1/synapse1.svg", "-p", "sc/out1/synapse1.hs"))
  
  @Test def synapse2() = 
    SCP1.main(Array("-si", "sc/synapse2.hs", "-t", "sc/out1/synapse2.svg", "-p", "sc/out1/synapse2.hs"))
  
  @Test def even() = 
    SCP1.main(Array("-si", "sc/even.hs", "-t", "sc/out1/even.svg", "-p", "sc/out1/even.hs"))
    
  @Test def even1() = 
    SCP1.main(Array("-si", "sc/even1.hs", "-t", "sc/out1/even1.svg", "-p", "sc/out1/even1.hs"))
  
  @Test def even2() = 
    SCP1.main(Array("-si", "sc/even2.hs", "-t", "sc/out1/even2.svg", "-p", "sc/out1/even2.hs"))
  
  @Test def evenSS() = 
    SCP1.main(Array("-si", "sc/evenSS.hs", "-t", "sc/out1/evenSS.svg", "-p", "sc/out1/evenSS.hs"))
  // -----
  @Test def case_() = 
    SCP1.main(Array("-si", "sc/case.hs", "-t", "sc/out1/case.svg", "-p", "sc/out1/case.hs"))
  
  @Test def eqnum() = 
    SCP1.main(Array("-si", "sc/eqnum.hs", "-t", "sc/out1/eqnum.svg", "-p", "sc/out1/eqnum.hs"))
  
  @Test def eqnumxx() = 
    SCP1.main(Array("-si", "sc/eqnumxx.hs", "-t", "sc/out1/eqnumxx.svg", "-p", "sc/out1/eqnumxx.hs"))
  
  @Test def exp() = 
    SCP1.main(Array("-si", "sc/exp.hs", "-t", "sc/out1/exp.svg", "-p", "sc/out1/exp.hs"))
  
  @Test def fib() = 
    SCP1.main(Array("-si", "sc/fib.hs", "-t", "sc/out1/fib.svg", "-p", "sc/out1/fib.hs"))
 
  @Test def filter() = 
    SCP1.main(Array("-si", "sc/filter.hs", "-t", "sc/out1/filter.svg", "-p", "sc/out1/filter.hs"))
  
  @Test def isort() = 
    SCP1.main(Array("-si", "sc/isort.hs", "-t", "sc/out1/isort.svg", "-p", "sc/out1/isort.hs"))
  
  @Test def leq_add() = 
    SCP1.main(Array("-si", "sc/leq_add.hs", "-t", "sc/out1/leq_add.svg", "-p", "sc/out1/leq_add.hs"))
  
  @Test def letrec_rev() = 
    SCP1.main(Array("-si", "sc/letrec_rev.hs", "-t", "sc/out1/letrec_rev.svg", "-p", "sc/out1/letrec_rev.hs"))
  
  @Test def letrec() = 
    SCP1.main(Array("-si", "sc/letrec.hs", "-t", "sc/out1/letrec.svg", "-p", "sc/out1/letrec.hs"))  
  
  @Test def min() = 
    SCP1.main(Array("-si", "sc/min.hs", "-t", "sc/out1/min.svg", "-p", "sc/out1/min.hs"))
  
  @Test def takenm() = 
    SCP1.main(Array("-si", "sc/takenm.hs", "-t", "sc/out1/takenm.svg", "-p", "sc/out1/takenm.hs"))
  
  @Test def war() = 
    SCP1.main(Array("-si", "sc/var.hs", "-t", "sc/out1/var.svg", "-p", "sc/out1/var.hs"))
  
  @Test def join1() = 
    SCP1.main(Array("-si", "sc/join1.hs", "-t", "sc/out1/join1.svg", "-p", "sc/out1/join1.hs"))
  
  @Test def join2() = 
    SCP1.main(Array("-si", "sc/join2.hs", "-t", "sc/out1/join2.svg", "-p", "sc/out1/join2.hs"))
  
  @Test def rev() = 
    SCP1.main(Array("-si", "sc/rev.hs", "-t", "sc/out1/rev.svg", "-p", "sc/out1/rev.hs"))
  
  @Test def church_mult() = {
    SCP1.main(Array("-si", "eq/churchMult1.hs", "-t", "sc/out1/churchMult1.svg", "-p", "sc/out1/churchMult1.hs"))
    SCP1.main(Array("-si", "eq/churchMult2.hs", "-t", "sc/out1/churchMult2.svg", "-p", "sc/out1/churchMult2.hs"))
  }
 
  @Test def ltl1 = 
    SCP1.main(Array("-si", "mc/ltl1.hs", "-t", "sc/out1/ltl1.svg", "-p", "sc/out1/ltl1.hs"))
}
