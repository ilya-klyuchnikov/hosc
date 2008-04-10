package hosc

import scala.util.parsing.input.{CharArrayReader, Reader}

import HLanguage._
import TermAlgebra._

class Interpreter(program: Program) {
  def this(fileName: String) = this(Util.programFromFile(fileName))
  
  def eval(t: Term): Term = lazyEval(t) match {
    case c: Constructor => Constructor(c.name, c.args.map(eval))
    case v => v
  }
  
  def eval(input: String): Term = {
    val pr = HParsers.parseTerm(new CharArrayReader(input.toCharArray))
    if (pr.isEmpty) throw new IllegalArgumentException(pr.toString)
    val term = pr.get
    Validator.valTerm(Set.empty[String] ++ (program.fs map {f => f.name}), term, program)
    Postprocessor.process(term, Set.empty[Variable])
    eval(term)
  }
  
  // Constructor or lambda as result
  private def lazyEval(t: Term): Term = null
  
}
