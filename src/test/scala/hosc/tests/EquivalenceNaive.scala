package hosc.tests

import org.junit.{ Test, Assert }

import hosc.Eq

class EquivalenceNaive extends Equivalence {
  import hosc.sc.NaiveSuperCompiler
  override def testEq(f1: String, f2: String): Unit = {
    val sc = NaiveSuperCompiler
    val p1 = sc.superCompileFile(examplesDir + f1)
    val p2 = sc.superCompileFile(examplesDir + f2)
    Assert.assertTrue(Eq.equivalent(p1.goal, p2.goal))
  }
}

class EquivalenceNaiveWithCoupling extends Equivalence {
  import hosc.sc.NaiveSuperCompilerWithCoupling
  override def testEq(f1: String, f2: String): Unit = {
    val sc = NaiveSuperCompilerWithCoupling
    val p1 = sc.superCompileFile(examplesDir + f1)
    val p2 = sc.superCompileFile(examplesDir + f2)
    Assert.assertTrue(Eq.equivalent(p1.goal, p2.goal))
  }
}

class EquivalenceNaiveWithControl extends Equivalence {
  import hosc.sc.NaiveSuperCompilerWithControl
  override def testEq(f1: String, f2: String): Unit = {
    val sc = NaiveSuperCompilerWithControl
    val p1 = sc.superCompileFile(examplesDir + f1)
    val p2 = sc.superCompileFile(examplesDir + f2)
    Assert.assertTrue(Eq.equivalent(p1.goal, p2.goal))
  }
}

class EquivalenceNaiveWithControlAndCoupling extends Equivalence {
  import hosc.sc.NaiveSuperCompilerWithControlAndCoupling
  override def testEq(f1: String, f2: String): Unit = {
    val sc = NaiveSuperCompilerWithControlAndCoupling
    val p1 = sc.superCompileFile(examplesDir + f1)
    val p2 = sc.superCompileFile(examplesDir + f2)
    Assert.assertTrue(Eq.equivalent(p1.goal, p2.goal))
  }
}

class EquivalenceNaiveTransientAware extends Equivalence {
  import hosc.sc.{ HOSC15, NaiveSuperCompilerTransientAware }
  override def testEq(f1: String, f2: String): Unit = {
    val sc = NaiveSuperCompilerTransientAware
    val p1 = sc.superCompileFile(examplesDir + f1)
    val p2 = sc.superCompileFile(examplesDir + f2)
    Assert.assertTrue(Eq.equivalent(p1.goal, p2.goal))
  }
}