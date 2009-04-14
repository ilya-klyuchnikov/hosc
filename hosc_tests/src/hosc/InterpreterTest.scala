package hosc

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._

import HLanguage._
import TestUtils._

class InterpreterTest {
  
  @Test def simple(): Unit = {
    testInt("hl/interpreter/rev1.hl", "Cons True (Cons False Nil)")
    testInt("hl/interpreter/rev2.hl", "Cons False (Cons True Nil)")
  }
  
  @Test def laziness(): Unit = {
    testInt("hl/interpreter/lazy.hl", "Cons Z (Cons (S Z) Nil)")
  }  
  
  def testInt(fileName: String, expectedOutput: String): Unit = {
    val in = new Interpreter(fileName)
    val expected = termFromString(expectedOutput)
    val actual = in.eval()
    assertEquals(expected, actual)
  }
}
