package hosc.service

import net.liftweb.http.{LiftRules, Req, PostRequest, S, XmlResponse}
import net.liftweb.util.Full
import hosc.sc0.ProcessTree0
import HParsers._
import hosc.sc0.SuperCompiler0
import hosc.sc0.ProcessTree0SVG
import hosc.sc0.CodeConstructor0
import hosc.LangUtils._
import hosc.HLanguage._
import scala.util.parsing.input.CharArrayReader

object AppService {
  val dispatcher: LiftRules.DispatchPF = {
    case Req(action::Nil, _, PostRequest) if action == "validate" || action == "run" => {
      () => {
        val result = HParsers.parseProgram(new CharArrayReader(S.param("program").openOr("").toCharArray))
        val xml = result match {
          case Success(program, _) => {
            val program1 = LambdaLifting.lift(program)
            findTypeErro(program1) match {
              case None => {
                val (residualCode, svg) = superCompile(program1)
                (<result status="ok"><code>{residualCode}</code><tree>{svg}</tree></result>)
              }
              case Some(typeError) => {
                (<result status="typeError"><details message={typeError.getMessage} /></result>)
              }
            }
          }
          case e:NoSuccess => {
            val msg = e.msg
            val pos = if (e.isInstanceOf[HError]) e.asInstanceOf[HError].pos.pos else e.next.pos  
            (<result status="parseError"><details message={msg} line={pos.line.toString()} column={pos.column.toString()} /></result>)
          }
        }
        Full(XmlResponse(xml))
      }
    }
  }
  
  def findTypeErro(program: HLanguage.Program): Option[TypeInferrer.TypeError] = {
    try {
      val ti = new TypeInferrer(program.ts)
      ti.inferType(hl0ToELC(program))
      None
    } catch {
      case e: TypeInferrer.TypeError => {
        Some(e)
      }
    }
  }
  
  def superCompile(program: HLanguage.Program): (String, scala.xml.Elem) = {
    val supercompiler = new SuperCompiler0(program)
    val processTree = supercompiler.buildProcessTree(program.goal)
    val svg = new ProcessTree0SVG(processTree).treeToSVG
    val generator = new CodeConstructor0(program, processTree, true)
    val residualProgram = generator.generateProgram()
    val residualProgramDocument = residualProgram.toDoc    
    val stringWriter = new java.io.StringWriter()
    residualProgramDocument.format(120, stringWriter)
    val residualCode = stringWriter.toString
    (residualCode, svg)
  }
}