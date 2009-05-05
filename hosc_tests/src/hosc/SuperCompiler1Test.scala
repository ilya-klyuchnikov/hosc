package hosc

import org.junit.Test
import hosc.{SuperCompilerApp1 => SCP1}

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
}
