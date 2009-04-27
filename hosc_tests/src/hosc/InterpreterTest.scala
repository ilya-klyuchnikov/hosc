package hosc

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._

import HLanguage._
import TestUtils._

class InterpreterTest {
  
  @Test def app(): Unit = {
    testInt("hl/interpreter/app.hs", "Cons True (Cons False Nil)")
  }
  
  @Test def rev(): Unit = {
    testInt("hl/interpreter/rev.hs", "Cons False (Cons True Nil)")
  }
  
  @Test def laziness(): Unit = {
    testInt("hl/interpreter/lazy.hs", "Cons Z (Cons (S Z) Nil)")
  }
  
  @Test def church(): Unit = {
    testInt("hl/interpreter/church.hs", "(S (Z ))")
  }
  
  def testInt(fileName: String, expectedOutput: String): Unit = {
    val in = new Interpreter(fileName)
    val expected = termFromString(expectedOutput)
    val actual = in.eval()
    assertEquals(expected, actual)
  }
}
