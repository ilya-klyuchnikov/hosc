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
}
