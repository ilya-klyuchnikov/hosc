package hosc.snippet;

import net.liftweb.http.S
import hosc.ProcessTree0SVG
import hosc.CodeConstructor0
import hosc.HLanguage._
import hosc.SuperCompiler0
import scala.util.parsing.input.CharArrayReader
import hosc.LangUtils._

class SC0 {
  def program = <pre>{S.param("program").openOr("")}</pre>
  def output = 
  try
  {
    val program = Util.programFromString(S.param("program").openOr(""))
    val program1 = LambdaLifting.lift(program)
    val ti = new TypeInferrer(program1.ts)
    ti.inferType(hl0ToELC(program1))
    val sc = new SuperCompiler0(program1)
    val pt = sc.buildProcessTree(program1.goal)
    val svg = new ProcessTree0SVG(pt).treeToSVG
    val g = new CodeConstructor0(program1, pt, true)
    val p1 = g.generateProgram()
    val doc1 = p1.toDoc    
    val writer1 = new java.io.StringWriter()
    doc1.format(120, writer1)
    <div>
    <h2>Supercompiled Expression</h2>
    <div><pre>{writer1.toString}</pre></div>
    <h2>Partial Process Tree</h2>
    <div>{svg}</div>
    </div>
  }
  catch {
    case e => 
    {
    e.printStackTrace();
    <div>
      <h2>Invalid Input</h2>
      <pre>{e.getMessage}</pre>
    </div>
    }
  }
}
