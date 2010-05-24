package hosc.spec

import org.junit.Test
import org.junit.Assert._

import hosc.Eq
import hosc.Util._
import hosc.{SuperCompilerApp => SCP}

/**
 * This is specification by examples.
 * Each input-output spec reflects in some way requirements for our zero-level supercompiler.
 */
class SuperCompilerSpec {
  
  @Test def iterate1 = 
    checkSpec("iterate1")
  
  @Test def iterate2 = 
    checkSpec("iterate2")
  
  @Test def russel1 = 
    checkSpec("russel1")
  
  @Test def russel2 = 
    checkSpec("russel2")
  
  @Test def russel3 = 
    checkSpec("russel3")
  
  @Test def russel4 = 
    checkSpec("russel4")
  
  @Test def russel5 = 
    checkSpec("russel5")
  
  def checkSpec(test: String): Unit = {
    val testIn = "spec/" + test + ".in.hs"
    val testOut = "spec/dump/" + test + ".hs"
    val testOutTree = "spec/dump/" + test + "svg"
    val specifiedOut = "spec/" + test + ".out.hs"
    
    SCP.main(Array("-si", testIn, "-t", testOutTree, "-p", testOut))
    
    val specOut = programFromFile(specifiedOut)
    val realOut = programFromFile(testOut)
    
    assertTrue("spec [" + test + "] should be satisfied", Eq.equivalent(specOut.goal, realOut.goal))
  }
}
