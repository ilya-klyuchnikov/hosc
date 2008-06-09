package hosc;
import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import TermAlgebra1._
import MSG1.msg
import HE1.he
import HLanguage1.{Application1 => A, Variable1 => V, CaseExpression1 => CE, Branch1 => B, Pattern1 => P,
  Constructor1 => C, LambdaAbstraction1 => LA, LetRecExpression1 => LR, _}

class MSG1Test {

  @Test def msg01(): Unit = {
    val appCall = V("app")
    appCall.call = true
    val msg_ = msg(appCall, appCall)
    println("XXXXXXXXXXXXXXX")
    println(appCall)
    println(appCall)
    println(msg_)
    assertEquals(msg_.term, appCall)
  }
  
  @Test def msg02(): Unit = {
    val appCall = V("app")
    appCall.call = true
    val revCall = V("rev")
    revCall.call = true
    val msg_ = msg(appCall, revCall)
    println("XXXXXXXXXXXXXXX")
    println(appCall)
    println(revCall)
    println(msg_)
  }
  
  @Test def msg03(): Unit = {
    val appCall1 = V("app")
    appCall1.call = true
    val appCall2 = V("app")
    appCall2.call = true
    
    val app1 = A(appCall1, C("S", V("x")::Nil))
    val app2 = A(appCall2, C("S", C("S", V("y")::Nil)::Nil))
    val msg_ = msg(app1, app2)
    println("XXXXXXXXXXXXXXX")
    println(app1)
    println(app2)
    println(msg_)
    
    app1.label = Repeat()
    val msg1_ = msg(app1, app2)
    println("XXXXXXXXXXXXXXX")
    println(app1)
    println(app2)
    println(msg1_)
  }
}
