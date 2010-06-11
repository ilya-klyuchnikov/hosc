package hosc.spec

import org.junit.Test
import org.junit.Assert._

import hosc.Eq
import hosc.Util._
import hosc.exp.{HigherLevelSuperCompilerApp => HLSC}

class HigherLevelSuperCompilerSpec {
  
  @Test def or_even_odd = 
    checkSpec("or_even_odd")
  
  @Test def even_doubleAcc = 
    checkSpec("even_doubleAcc")
  
  @Test def and_even_odd = 
    checkSpec("and_even_odd")
  
  @Test def even_plus_xx = 
    checkSpec("even_plus_xx")

  def checkSpec(test: String): Unit = {
    val testIn = "spec_hl/" + test + ".in.hs"
    val testOut = "spec_hl/dump/" + test + ".hs"
    val testOutTree = "spec_hl/dump/" + test + ".svg"
    val specifiedOut = "spec_hl/" + test + ".out.hs"
    
    HLSC.main(Array("-si", testIn, "-t", testOutTree, "-p", testOut))
    
    val specOut = programFromFile(specifiedOut)
    val realOut = programFromFile(testOut)
    
    assertTrue("spec [" + test + "] should be satisfied", Eq.equivalent(specOut.goal, realOut.goal))
  }
}
