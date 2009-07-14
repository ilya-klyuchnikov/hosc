package hosc

import org.junit.Test
import org.junit.Assert._
import TypeInferrer._
import scala.util.parsing.input.{CharArrayReader, Reader}

class TypeInferrerTest {

  @Test def case1() = 
    testTyping("types/case1.hs",
               "Pair a b -> a")
  
  @Test def case2() = 
    testTyping("types/case2.hs",
               "Pair a b -> b")
  
  @Test def church() = 
    testTyping("types/church.hs",
               "Nat -> (t -> t) -> (t -> t)")
  
  @Test def churchAdd() = 
    testTyping("types/churchAdd.hs", 
               "(t -> t2 -> t3) -> (t -> t1 -> t2) -> (t -> t1 -> t3)")
  
  @Test def churchSub() = 
    testTyping("types/churchSub.hs",
               "t7 -> ((( ((t -> t1) -> (t1 -> t2) -> t2) -> (t3 -> t4) -> (t5 -> t5) -> t6) -> t -> t4 -> t6) -> t7 -> t8) -> t8")
  
  @Test def churchSubErr() = 
    testTypeError("types/churchSubErr.hs")
  
  @Test def fixErr() =
    testTypeError("types/fixErr.hs")
  
  def testTyping(fileName: String, typeString: String) = {
    val actualType = Util.inferGoalType(fileName)
    val expectedType = HParsers.parseType(new CharArrayReader(typeString.toCharArray)).get
    assertTrue(TypeInferrer.equivalent(actualType, expectedType))
  }
  
  def testTypeError(fileName: String) = {
    try {
      val actualType = Util.inferGoalType(fileName)
      fail(fileName + " cannot be well-typed")
    } catch {
      case e: TypeError => 
    }
  }
}
