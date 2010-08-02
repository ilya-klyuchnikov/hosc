package hosc

import org.junit.Test
import org.junit.Assert._
import TestUtils._

class InterpreterTest {

  val examplesDir = "examples/"
  @Test
  def app() = testInt("interpreter/app.hs", "Cons True (Cons False Nil)")
  @Test
  def rev() = testInt("interpreter/rev.hs", "Cons False (Cons True Nil)")
  @Test
  def laziness() = testInt("interpreter/lazy.hs", "Cons Z (Cons (S Z) Nil)")
  @Test
  def church() = testInt("interpreter/church.hs", "(S (Z ))")

  def testInt(fileName: String, expectedOutput: String): Unit = {
    val in = new Interpreter(examplesDir + fileName)
    val expected = termFromString(expectedOutput)
    val actual = in.eval()
    assertEquals(expected, actual)
  }
}
