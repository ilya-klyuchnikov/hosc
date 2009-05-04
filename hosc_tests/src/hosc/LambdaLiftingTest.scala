package hosc

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._

import HLanguage._
import TestUtils._

class LambdaLiftingTest {
  @Test def letrec1(): Unit = {
    val p = Util.rawProgramFromFile("lifting/letrec.hs")
    println(p.goal)
    val l = LambdaLifting.findLetRec(p.goal)
    println(l)
    println("\n")
    
    val e = LambdaLifting.lift(p)
    println(e)
  }
  
  @Test def letrec_rev(): Unit = {
    val p = Util.rawProgramFromFile("lifting/letrec_rev.hs")
    println(p.goal)
    val l = LambdaLifting.findLetRec(p.goal)
    println(l)
    
    println("\n")
    
    val e = LambdaLifting.lift(p)
    println(e)
  }
}
