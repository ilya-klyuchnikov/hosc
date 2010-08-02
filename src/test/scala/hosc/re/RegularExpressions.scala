package hosc.re

import org.junit.{Test, Assert}
import org.junit.Assert._

import hosc.TUtils._
import hosc.{SuperCompilerApp => SCP}
import hosc.exp.{SuperCompilerApp1 => SCP1, HigherLevelSuperCompilerApp => HLSCP}
import hosc.Interpreter

class RegularExpressionsTest {
  
  //@Test
  def re1:Unit = {
    SCP.main(Array("-si", "examples/re/re1.hs", "-t", "examples/re/out/re1.svg", "-p", "examples/re/out/re1.hs"))
  }
  
  //@Test
  def re2:Unit = {
    SCP.main(Array("-si", "examples/re/re3.hs", "-t", "examples/re/out/re3.svg", "-p", "examples/re/out/re3.hs"))
  }
  
  @Test
  def re4:Unit = {
    SCP.main(Array("-si", "examples/re/regexp2.hs", "-t", "examples/re/out/re6.svg", "-p", "examples/re/out/re6.hs"))
  }
  
  @Test
  def arith:Unit = {
    SCP.main(Array("-si", "examples/re/arith.hs", "-t", "examples/re/out/arith.svg", "-p", "examples/re/out/arith.hs"))
  }
  
  @Test
  def arith1:Unit = {
    SCP.main(Array("-si", "examples/re/arith1.hs", "-t", "examples/re/out/arith1.svg", "-p", "examples/re/out/arith1.hs"))
  }
  
  @Test
  def p1:Unit = {
    SCP.main(Array("-si", "examples/re/post/in/p1.hs", "-t", "examples/re/post/out/p1.svg", "-p", "examples/re/post/out/p1.hs"))
  }
  
  def testInt(fileName: String, expectedOutput: String): Unit = {
    val in = new Interpreter(fileName)
    val expected = termFromString(expectedOutput)
    val actual = in.eval()
    assertEquals(expected, actual)
  }
}
