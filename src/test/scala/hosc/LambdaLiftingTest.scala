package hosc

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._

import HLanguage._
import TUtils._

class LambdaLiftingTest {
  val examplesDir = "examples/"

  @Test
  def letrec1(): Unit = {
    val p = Util.rawProgramFromFile(examplesDir + "lifting/letrec.hs")
    println(p.goal)
    val l = LambdaLifting.findLetRec(p.goal)
    println(l)
    println("\n")

    val e = LambdaLifting.lift(p)
    println(e.toDocString)
  }

  @Test
  def letrec_rev(): Unit = {
    val p = Util.rawProgramFromFile(examplesDir + "lifting/letrec_rev.hs")
    println(p.goal)
    val l = LambdaLifting.findLetRec(p.goal)
    println(l)

    println("\n")

    val e = LambdaLifting.lift(p)
    println(e.toDocString)
  }

  @Test
  def letrec_2app(): Unit = {
    val p = Util.rawProgramFromFile(examplesDir + "lifting/letrec_2app.hs")
    println(p.goal)
    val l = LambdaLifting.findLetRec(p.goal)
    println(l)

    println("\n")

    val e = LambdaLifting.lift(p)
    println(e.toDocString)
  }
}
