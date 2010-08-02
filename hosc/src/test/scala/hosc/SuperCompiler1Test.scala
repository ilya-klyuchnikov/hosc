package hosc

import org.junit.Test
import hosc.exp.{ SuperCompilerApp1 => SCP1 }

class SuperCompiler1Test {
  val examples_dir = "examples/"
	  
  @Test
  def t01_loop =
    SCP1.main(Array("-si", examples_dir + "sc/01_loop.hs", "-t", examples_dir + "sc/out1/01_loop.svg", "-p", examples_dir + "sc/out1/01_loop.hs"))

  @Test
  def loop =
    SCP1.main(Array("-si", examples_dir + "sc/loop.hs", "-t", examples_dir + "sc/out1/loop.svg", "-p", examples_dir + "sc/out1/loop.hs"))

  @Test
  def eqnum_plus =
    SCP1.main(Array("-si", examples_dir + "sc/eqnum_plus.hs", "-t", examples_dir + "sc/out1/eqnum_plus.svg", "-p", examples_dir + "sc/out1/eqnum_plus.hs"))

  @Test
  def eq =
    SCP1.main(Array("-si", examples_dir + "sc/eq.hs", "-t", examples_dir + "sc/out1/eq.svg", "-p", examples_dir + "sc/out1/eq.hs"))

  @Test
  def zip1 =
    SCP1.main(Array("-si", examples_dir + "sc/zip1.hs", "-t", examples_dir + "sc/out1/zip1.svg", "-p", examples_dir + "sc/out1/zip1.hs"))

  @Test
  def zip2 =
    SCP1.main(Array("-si", examples_dir + "sc/zip2.hs", "-t", examples_dir + "sc/out1/zip2.svg", "-p", examples_dir + "sc/out1/zip2.hs"))

  @Test
  def regexp(): Unit =
    SCP1.main(Array("-si", examples_dir + "sc/regexp.hs", "-t", examples_dir + "sc/out1/regexp.svg", "-p", examples_dir + "sc/out1/regexp.hs"))

  @Test
  def synapse() =
    SCP1.main(Array("-si", examples_dir + "sc/synapse.hs", "-t", examples_dir + "sc/out1/synapse.svg", "-p", examples_dir + "sc/out1/synapse.hs"))

  @Test
  def synapse1() =
    SCP1.main(Array("-si", examples_dir + "sc/synapse1.hs", "-t", examples_dir + "sc/out1/synapse1.svg", "-p", examples_dir + "sc/out1/synapse1.hs"))

  @Test
  def synapse2() =
    SCP1.main(Array("-si", examples_dir + "sc/synapse2.hs", "-t", examples_dir + "sc/out1/synapse2.svg", "-p", examples_dir + "sc/out1/synapse2.hs"))

  @Test
  def even() =
    SCP1.main(Array("-si", examples_dir + "sc/even.hs", "-t", examples_dir + "sc/out1/even.svg", "-p", examples_dir + "sc/out1/even.hs"))

  @Test
  def even1() =
    SCP1.main(Array("-si", examples_dir + "sc/even1.hs", "-t", examples_dir + "sc/out1/even1.svg", "-p", examples_dir + "sc/out1/even1.hs"))

  @Test
  def even2() =
    SCP1.main(Array("-si", examples_dir + "sc/even2.hs", "-t", examples_dir + "sc/out1/even2.svg", "-p", examples_dir + "sc/out1/even2.hs"))

  @Test
  def evenSS() =
    SCP1.main(Array("-si", examples_dir + "sc/evenSS.hs", "-t", examples_dir + "sc/out1/evenSS.svg", "-p", examples_dir + "sc/out1/evenSS.hs"))
  // -----
  @Test
  def case_() =
    SCP1.main(Array("-si", examples_dir + "sc/case.hs", "-t", examples_dir + "sc/out1/case.svg", "-p", examples_dir + "sc/out1/case.hs"))

  @Test
  def eqnum() =
    SCP1.main(Array("-si", examples_dir + "sc/eqnum.hs", "-t", examples_dir + "sc/out1/eqnum.svg", "-p", examples_dir + "sc/out1/eqnum.hs"))

  @Test
  def eqnumxx() =
    SCP1.main(Array("-si", examples_dir + "sc/eqnumxx.hs", "-t", examples_dir + "sc/out1/eqnumxx.svg", "-p", examples_dir + "sc/out1/eqnumxx.hs"))

  @Test
  def exp() =
    SCP1.main(Array("-si", examples_dir + "sc/exp.hs", "-t", examples_dir + "sc/out1/exp.svg", "-p", examples_dir + "sc/out1/exp.hs"))

  @Test
  def fib() =
    SCP1.main(Array("-si", examples_dir + "sc/fib.hs", "-t", examples_dir + "sc/out1/fib.svg", "-p", examples_dir + "sc/out1/fib.hs"))

  @Test
  def filter() =
    SCP1.main(Array("-si", examples_dir + "sc/filter.hs", "-t", examples_dir + "sc/out1/filter.svg", "-p", examples_dir + "sc/out1/filter.hs"))

  @Test
  def isort() =
    SCP1.main(Array("-si", examples_dir + "sc/isort.hs", "-t", examples_dir + "sc/out1/isort.svg", "-p", examples_dir + "sc/out1/isort.hs"))

  @Test
  def leq_add() =
    SCP1.main(Array("-si", examples_dir + "sc/leq_add.hs", "-t", examples_dir + "sc/out1/leq_add.svg", "-p", examples_dir + "sc/out1/leq_add.hs"))

  @Test
  def letrec_rev() =
    SCP1.main(Array("-si", examples_dir + "sc/letrec_rev.hs", "-t", examples_dir + "sc/out1/letrec_rev.svg", "-p", examples_dir + "sc/out1/letrec_rev.hs"))

  @Test
  def letrec() =
    SCP1.main(Array("-si", examples_dir + "sc/letrec.hs", "-t", examples_dir + "sc/out1/letrec.svg", "-p", examples_dir + "sc/out1/letrec.hs"))

  @Test
  def min() =
    SCP1.main(Array("-si", examples_dir + "sc/min.hs", "-t", examples_dir + "sc/out1/min.svg", "-p", examples_dir + "sc/out1/min.hs"))

  @Test
  def takenm() =
    SCP1.main(Array("-si", examples_dir + "sc/takenm.hs", "-t", examples_dir + "sc/out1/takenm.svg", "-p", examples_dir + "sc/out1/takenm.hs"))

  @Test
  def war() =
    SCP1.main(Array("-si", examples_dir + "sc/var.hs", "-t", examples_dir + "sc/out1/var.svg", "-p", examples_dir + "sc/out1/var.hs"))

  @Test
  def join1() =
    SCP1.main(Array("-si", examples_dir + "sc/join1.hs", "-t", examples_dir + "sc/out1/join1.svg", "-p", examples_dir + "sc/out1/join1.hs"))

  @Test
  def join2() =
    SCP1.main(Array("-si", examples_dir + "sc/join2.hs", "-t", examples_dir + "sc/out1/join2.svg", "-p", examples_dir + "sc/out1/join2.hs"))

  @Test
  def rev() =
    SCP1.main(Array("-si", examples_dir + "sc/rev.hs", "-t", examples_dir + "sc/out1/rev.svg", "-p", examples_dir + "sc/out1/rev.hs"))

  @Test
  def church_mult() = {
    SCP1.main(Array("-si", examples_dir + "eq/churchMult1.hs", "-t", examples_dir + "sc/out1/churchMult1.svg", "-p", examples_dir + "sc/out1/churchMult1.hs"))
    SCP1.main(Array("-si", examples_dir + "eq/churchMult2.hs", "-t", examples_dir + "sc/out1/churchMult2.svg", "-p", examples_dir + "sc/out1/churchMult2.hs"))
  }

  @Test
  def ltl1 =
    SCP1.main(Array("-si", examples_dir + "mc/ltl1.hs", "-t", examples_dir + "sc/out1/ltl1.svg", "-p", examples_dir + "sc/out1/ltl1.hs"))
}
