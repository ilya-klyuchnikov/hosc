package hosc.spec

import org.junit.Test
import org.junit.Assert._

import hosc.Eq
import hosc.Util._

/**
 * This is specification by examples.
 * Each input-output spec reflects in some way requirements for our zero-level supercompiler.
 */
class SuperCompilerSpec15 extends SuperCompilerSpec{

  import hosc.sc.HOSC15
  override def checkSpec(test: String): Unit = {
    val testIn = examplesDir + "spec/" + test + ".in.hs"
    val testOut = examplesDir + "spec/dump/" + test + ".hs"
    val testOutTree = examplesDir + "spec/dump/" + test + ".svg"
    val specifiedOut = examplesDir + "spec/" + test + ".out.hs"

    HOSC15.superCompileFile2File(testIn, testOut)

    val specOut = programFromFile(specifiedOut)
    val realOut = programFromFile(testOut)

    assertTrue("spec [" + test + "] should be satisfied", Eq.equivalent(specOut.goal, realOut.goal))
  }
}
