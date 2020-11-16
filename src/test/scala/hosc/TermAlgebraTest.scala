package hosc

import org.junit.Test
import org.junit.Assert._
import HLanguage.{Application => A, _}
import TermAlgebra._
import TUtils._
import Util._

class TermAlgebraTest {

  val inputFile = "examples/term_algebra/test.hs"

  @Test def equivalency01(): Unit = {
    val appCall = Variable("app")
    appCall.global = true
    assertTrue(equivalent(appCall, appCall))
  }

  @Test def equivalency02(): Unit = {
    val appCall = Variable("app")
    appCall.global = true
    val revCall = Variable("rev")
    appCall.global = true
    assertFalse(equivalent(appCall, revCall))
  }

  @Test def equivalency03(): Unit = {
    val appCall = Variable("app")
    appCall.global = true
    assertFalse(equivalent(appCall, Variable("x")))
  }

  @Test def equivalency04(): Unit = {
    val appCall = Variable("app")
    appCall.global = true
    assertFalse(equivalent(Variable("y"), appCall))
  }

  @Test def equivalency05(): Unit = {
    assertTrue(equivalent(Variable("y"), Variable("x")))
  }

  @Test def equivalency06(): Unit = {
    val program = programFromFile(inputFile)
    val f1 = program.getFunction("f1").get
    val f2 = program.getFunction("f2").get
    assertTrue(equivalent(f1.body, f2.body))
  }

  @Test def equivalency07(): Unit = {
    val program = programFromFile(inputFile)
    val term1 = termFromString("app (app x y) z", program)
    val term2 = termFromString("app (app x y) x", program)
    assertFalse(equivalent(term1, term2))
  }
}
