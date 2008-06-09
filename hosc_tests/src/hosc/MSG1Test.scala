package hosc;
import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import TermAlgebra1._
import MSG1._
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
    assertTrue(instanceOf(app1, app2))
    
    app1.label = Repeat()
    val msg1_ = msg(app1, app2)
    println("XXXXXXXXXXXXXXX")
    println(app1)
    println(app2)
    println(msg1_)
  }
  
  @Test def msg04(): Unit = {
    val f1 = V("f1")
    val f1Call = V("f1"); f1Call.call = true
    val lr1 = LR((f1Call, LA(V("y"), A(f1Call, V("y")))), A(f1Call, V("x2")))
    
    val f2 = V("f2")
    val f2Call = V("f2"); f2Call.call = true
    val lr2 = LR((f2Call, LA(V("y"), A(f2Call, V("y")))), A(f2Call, V("x1")))
    
    val msg_ = msg(lr1, lr2)
    val smsg_ = strongMsg(lr1, lr2)
    println("XXXXXXXXXXXXXXX")
    println(lr1)
    println(lr2)
    println(msg_)
    println(smsg_)
    assertTrue(instanceOf(lr1, lr2))
  }
  
  @Test def msg05(): Unit = {
    val f1Call = V("f1"); f1Call.call = true
    val la1 = LA(V("y"), A(f1Call, V("y")))
    val f2 = V("f2")
    val f2Call = V("f1"); f2Call.call = true
    val la2 = LA(V("z"), A(f2Call, V("z")))
    
    val msg_ = msg(la1, la2)
    val smsg_ = strongMsg(la1, la2)
    println("XXXXXXXXXXXXXXX")
    println(la1)
    println(la2)
    println(msg_)
    println(smsg_)
    assertTrue(instanceOf(la1, la2))
  }
}
