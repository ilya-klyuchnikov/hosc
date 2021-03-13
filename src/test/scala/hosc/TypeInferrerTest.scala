package hosc

import org.junit.Assert._
import org.junit.Test

class TypeInferrerTest {

  @Test def case1(): Unit =
    testTyping(
      "types/case1.hs",
      "Pair a b -> a",
    )

  @Test def case2(): Unit =
    testTyping(
      "types/case2.hs",
      "Pair a b -> b",
    )

  @Test def church(): Unit =
    testTyping(
      "types/church.hs",
      "Nat -> (t -> t) -> (t -> t)",
    )

  @Test def churchAdd(): Unit =
    testTyping(
      "types/churchAdd.hs",
      "(t -> t2 -> t3) -> (t -> t1 -> t2) -> (t -> t1 -> t3)",
    )

  @Test def churchSub(): Unit =
    testTyping(
      "types/churchSub.hs",
      "t7 -> (((((t -> t1) -> (t1 -> t2) -> t2) -> (t3 -> t4) -> (t5 -> t5) -> t6) -> t -> t4 -> t6) -> t7 -> t8) -> t8",
    )

  @Test def churchSubErr(): Unit =
    testTypeError("types/churchSubErr.hs")

  @Test def fixErr(): Unit =
    testTypeError("types/fixErr.hs")

  @Test def drec(): Unit =
    println(Util.inferGoalType("examples/sc/encoding.hs"))

  @Test def evalErr(): Unit =
    testTypeError("types/evalErr.hs")

  @Test def eval(): Unit =
    testTyping("types/eval.hs", "Exp -> Val")

  def testTyping(fileName: String, typeString: String): Unit = {
    val actualType = Util.inferGoalType("examples/" + fileName)
    val expectedType = TUtils.typeResultFromString(typeString).get
    println("actual:")
    println(actualType)
    println("expected:")
    println(expectedType)
    assertTrue(TypeAlgebra.equivalent(actualType, expectedType))
  }

  def testTypeError(fileName: String): Unit = {
    try {
      val actualType = Util.inferGoalType(fileName)
      fail(fileName + " cannot be well-typed")
    } catch {
      case e: Throwable =>
    }
  }
}
