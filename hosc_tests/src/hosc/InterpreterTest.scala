package hosc

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import HLanguage._
import TestUtils._

class InterpreterTest {
  
  @Test def simple(): Unit = {
    testInt("input/revInt.hl", "app (Cons True Nil) (Cons False Nil)", "Cons True (Cons False Nil)")
    testInt("input/revInt.hl", "rev (Cons True (Cons False Nil))", "Cons False (Cons True Nil)")
  }
  
  @Test def laziness(): Unit = {
    testInt("input/lazy.hl", "take (S (S Z)) (from Z)", "Cons Z (Cons (S Z) Nil)")
  }
  
  
  def testInt(fileName: String, input: String, expectedOutput: String): Unit = {
    val in = new Interpreter(fileName)
    val expected = termFromString(expectedOutput)
    val actual = in.eval(input)
    assertEquals(expected, actual)
  }
}