package hosc.tests

import org.junit.Test

import hosc.sc.{SuperCompilerApp, SuperCompilerWithControlApp}
/**
 * Tests ensuring that supercompilation of simple examples 
 * terminates in a reasonable time
 */
class Termination {
  @Test//{val timeout=5000}
  def ack = 
    run("hll/ack.hs")
  
  @Test//{val timeout=5000}
  def eqnumxx = 
    run("hll/eqnumxx.hs")
  
  @Test//{val timeout=5000}
  def even_double_acc = 
    run("hll/even_double_acc.hs")
  
  @Test//{val timeout=5000}
  def fib =
    run("hll/fib.hs")
  
  @Test//{val timeout=5000}
  def filter = 
    run("hll/filter.hs")
  
  @Test//{val timeout=5000}
  def fix_con = 
    run("hll/fix_con.hs")
  
  @Test//{val timeout=5000}
  def isort = 
    run("hll/isort.hs")
  
  @Test//{val timeout=5000}
  def rev = 
    run("hll/rev.hs")
  
  @Test//{val timeout=5000}
  def synapse = 
    run("hll/synapse.hs")
  
  def run(file: String): Unit = {
    SuperCompilerApp.superCompileFile(file)
    SuperCompilerWithControlApp.superCompileFile(file)
  }
}