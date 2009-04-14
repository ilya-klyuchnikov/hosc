package hosc.snippet;

import net.liftweb.http.S
import hosc.ProcessTreeSVG
import hosc.CodeConstructor
import hosc.HLanguage._
import hosc.Eq
import hosc.SuperCompiler
import scala.util.parsing.input.CharArrayReader
import hosc.LangUtils._

class Eqt {
  def types = S.param("types").openOr("")
  def goal1 = S.param("goal1").openOr("")
  def goal2 = S.param("goal2").openOr("")
  def defs = S.param("defs").openOr("")
  
  def types_ = <pre>{types}</pre>
  def goal1_ = <pre>{goal1}</pre>
  def goal2_ = <pre>{goal2}</pre>
  def defs_ = <pre>{defs}</pre>
  
  
  def result = 
  try
  {
    
    val text1 = types + goal1 + " where " + defs
    val text2 = types + goal2 + " where " + defs
    val program1 = LambdaLifting.lift(Util.programFromString(text1))
    val program2 = LambdaLifting.lift(Util.programFromString(text2))
    
    val ti = new TypeInferrer(program1.ts)
    ti.inferType(hl0ToELC(program1))
    
    val sc1 = new SuperCompiler(program1)
    val pt1 = sc1.buildProcessTree(program1.goal)
    val g1 = new CodeConstructor(program1, pt1, true)
    val p1 = g1.generateProgram()
    
    val sc2 = new SuperCompiler(program2)
    val pt2 = sc1.buildProcessTree(program2.goal)
    val g2 = new CodeConstructor(program2, pt2, true)
    val p2 = g2.generateProgram()
    
    val doc1 = p1.toDoc    
    val writer1 = new java.io.StringWriter()
    doc1.format(120, writer1)
    
    val doc2 = p2.toDoc    
    val writer2 = new java.io.StringWriter()
    doc2.format(120, writer2)
    
    val eq = Eq.equivalent(p1.goal, p2.goal)
    val h = if (eq) "Equivalent!" else "Can not infer equivalence"
    
    <div>
    <h2>{h}</h2>
    <h3>Residual program 1</h3>
    <div><pre>{writer1.toString}</pre></div>
    <h3>Residual program 2</h3>
    <div><pre>{writer2.toString}</pre></div>
    </div>
  }
  catch {
    case e => 
    {
    <div>
      <h2>Invalid Input</h2>
      <pre>{e.getMessage}</pre>
    </div>
    }
  }
}
