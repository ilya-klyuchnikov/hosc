package hosc.service

import net.liftweb.http.{LiftRules, Req, PostRequest, S, XmlResponse}
import net.liftweb.util.Full
import hosc.ProcessTree
import HParsers._
import hosc.SuperCompiler
import hosc.ProcessTreeSVG
import hosc.CodeConstructor
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
    case Req(action::Nil, _, PostRequest) if action == "eq" => {
      () => {
        val types = S.param("types").openOr("")
        val goal1 = S.param("goal1").openOr("")
        val goal2 = S.param("goal2").openOr("")
        val defs = S.param("defs").openOr("")
        val xml = 
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
		    val h: String = if (eq) "true" else "false"
		    
            (<result status="ok" eq={eq.toString} >
              <code1>{writer1.toString}</code1>
              <code2>{writer2.toString}</code2>
            </result>)
		  }
		  catch {
		    case e => 
		      <result status="typeError"><details message={e.getMessage} /></result>
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
    val supercompiler = new SuperCompiler(program)
    val processTree = supercompiler.buildProcessTree(program.goal)
    val svg = new ProcessTreeSVG(processTree).treeToSVG
    val generator = new CodeConstructor(program, processTree, true)
    val residualProgram = generator.generateProgram()
    val residualProgramDocument = residualProgram.toDoc    
    val stringWriter = new java.io.StringWriter()
    residualProgramDocument.format(120, stringWriter)
    val residualCode = stringWriter.toString
    (residualCode, svg)
  }
}