package hosc.snippet;

import net.liftweb.http.S
import hosc.sc1.ProcessTree1SVG
import hosc.HLanguage1._
import hosc.sc1.{SuperCompiler1, InputUtil1, ProcessTree1, VarGen1}
import scala.util.parsing.input.CharArrayReader
import hosc.LangUtils._

class SC1 {
  def program = <pre>{S.param("program").openOr("")}</pre>
  def output = 
  try
  {
    val program = InputUtil1.program1FromString(S.param("program").openOr(""))
    val ti = new TypeInferrer(program.ts)
    ti.inferType(hl1ToELC(program.expr))
    val sc = new SuperCompiler1(program, new VarGen1())
    val (pt:ProcessTree1, resProgram:Program1) = sc.superCompile() 
    val svg = new ProcessTree1SVG(pt).treeToSVG
    val doc1 = resProgram.toDoc    
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
