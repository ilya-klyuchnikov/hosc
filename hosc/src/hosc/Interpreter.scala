package hosc

import scala.util.parsing.input.{CharArrayReader, Reader}

import HLanguage._
import TermAlgebra._

class Interpreter(program: Program) {
  var i = 0
  def newVar() = {
    i += 1
    Variable("$" + i) 
  }
  
  def this(fileName: String) = this(Util.programFromFile(fileName))
  
  def eval(t: Term): Term = lazyEval(t) match {
    case c: Constructor => Constructor(c.name, c.args.map(eval))
    case l: LambdaAbstraction => l
    case x => throw new Exception("Internal Error: lazy eval returns " + x)
  }
  
  def eval(input: String): Term = {
    val pr = HParsers.parseTerm(new CharArrayReader(input.toCharArray))
    if (pr.isEmpty) throw new IllegalArgumentException(pr.toString)
    val term = pr.get
    Validator.valTerm(Set.empty[String] ++ (program.fs map {f => f.name}), term, program)
    Postprocessor.process(term, Set.empty[Variable])
    eval(term)
  }
  
  private def lazyEval(term: Term): Term = {
    var t = term
    do {
      t = baseLazyEval(t);      
    } while (decompose(t).isInstanceOf[Context])
    t
  }
  
  // Constructor or lambda as result
  private def baseLazyEval(t: Term): Term = {    
    decompose(t) match {
    case o: Observable => o.term
    case context: Context => context.redex match {
      case RedexCall(v) => {
        val lam = program.getFunction(v.name).get.lam
        val nv = newVar()
        val nl = LambdaAbstraction(nv, applySubstitution(lam.t, Map(lam.v -> nv)))
        context.replaceHole(nl) 
      }
      case RedexLamApp(lam, app) => context.replaceHole(applySubstitution(lam.t, Map(lam.v -> app.arg)))
      case RedexCaseCon(c, ce) => {
        val b = ce.branches.find(_.pattern.name == c.name).get
        val sub = Map[Variable, Term]() ++ (b.pattern.args zip c.args)
        context.replaceHole(applySubstitution(b.term, sub))
      }
      case _ => throw new Exception("Unexpexted redex is encoutered " + context.redex)
    }
  }
  }
  
}
