package hosc.re

import org.junit.{Test, Assert}
import org.junit.Assert._

import hosc.TestUtils._
import hosc.{SuperCompilerApp => SCP}
import hosc.exp.{SuperCompilerApp1 => SCP1, HigherLevelSuperCompilerApp => HLSCP}
import hosc.Interpreter

class RegularExpressions {
  //@Test
  def interpret = {
    
  }
  
  //@Test
  def re1:Unit = {
    SCP.main(Array("-si", "re/re1.hs", "-t", "re/out/re1.svg", "-p", "re/out/re1.hs"))
  }
  
  //@Test
  def re2:Unit = {
    SCP.main(Array("-si", "re/re3.hs", "-t", "re/out/re3.svg", "-p", "re/out/re3.hs"))
  }
  
  @Test
  def re4:Unit = {
    SCP.main(Array("-si", "re/regexp2.hs", "-t", "re/out/re6.svg", "-p", "re/out/re6.hs"))
  }
  
  @Test
  def arith:Unit = {
    SCP.main(Array("-si", "re/arith.hs", "-t", "re/out/arith.svg", "-p", "re/out/arith.hs"))
  }
  
  @Test
  def arith1:Unit = {
    SCP.main(Array("-si", "re/arith1.hs", "-t", "re/out/arith1.svg", "-p", "re/out/arith1.hs"))
  }
  
  @Test
  def p1:Unit = {
    SCP.main(Array("-si", "re/post/in/p1.hs", "-t", "re/post/out/p1.svg", "-p", "re/post/out/p1.hs"))
  }
  
  def testInt(fileName: String, expectedOutput: String): Unit = {
    val in = new Interpreter(fileName)
    val expected = termFromString(expectedOutput)
    val actual = in.eval()
    assertEquals(expected, actual)
  }
}
