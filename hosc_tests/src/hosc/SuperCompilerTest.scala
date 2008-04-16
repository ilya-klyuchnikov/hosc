package hosc

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import HLanguage._
import Util._

class SuperCompilerTest {
  
  @Test def simple(): Unit = {
    testSC("input/revInt.hl", "app (app x y) z")
    testSC("input/revInt.hl", "app (app x y) x")
    testSC("input/revInt.hl", "app x x")
  }  
  
  def testSC(fileName: String, input: String): Unit = {
    val p = programFromFile(fileName)
    val sc = new SuperCompiler(p)
    val term = termFromString(input, p)
    val tree = sc.buildProcessTree(term)
    println(tree)
  }
}
