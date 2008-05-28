package hosc.snippet;

import net.liftweb.http.S
import hosc.ProcessTreeSVG
import hosc.ResidualProgramGenerator
import hosc.HLanguage._
import hosc.SuperCompiler
import scala.util.parsing.input.CharArrayReader

class HOSC {
  def program = <pre>{S.param("program").openOr("")}</pre>
  def output = 
  try
  {
    val program = Util.programFromString(S.param("program").openOr(""))
    val ti = new TypeInferrer(program)
    
    ti.tcProgram()
    val sc = new SuperCompiler(program)
    val pt = sc.buildProcessTree(program.goal)
    val svg = new ProcessTreeSVG(pt).treeToSVG
    val g = new ResidualProgramGenerator(pt)
    val doc = g.generateProgram().toDoc
    val writer = new java.io.StringWriter()
    doc.format(120, writer)
    <div>
    <h2>Supercompiled Expression</h2>
    <div><pre>{writer.toString}</pre></div>
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
