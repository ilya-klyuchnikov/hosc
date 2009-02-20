package hosc.sc0

import scala.util.parsing.input.{Positional, Reader}
import scala.util.parsing.combinator.ImplicitConversions
import HLanguage._

object HParsers0 extends HTokenParsers with StrongParsers with ImplicitConversions {
  
  lexical.delimiters += ("(", ")", ",", "=", ";", "{", "}", "::", "|", "->", "\\")
  lexical.reserved += ("case", "of", "where", "data")
  
  def program = (typeConstrDefinition*) ~ term ~ ("where" ~> strongRep1(function)|success(Nil)) ^^ Program
  def function = p(lident ~ ("=" ~> lambdaAbstraction <~ c(";")) ^^ Function)
  
  def term: Parser[Term] = p(tr2 | appl) | err("term is expected")
  def appl = chainl1(tr0, tr1, success(Application(_: Term, _: Term)))
    
  // head of application
  private def tr0: Parser[Term] = p(variable | lambdaAbstraction | caseExpression |("(" ~> appl <~ ")"))
  // argument of or application constructor
  private def tr1 = p(tr0 | uident ^^ {x => Constructor(x, Nil)} | ("(" ~> term <~ ")"))
  // top constructor; cannot be head of application
  private def tr2: Parser[Constructor] =  p(uident ~ (tr1*) ^^ Constructor | ("(" ~> tr2 <~ ")"))
  
  private def variable = p(lident ^^ Variable)  
  private def lambdaAbstraction = p("\\" ~> c(variable+) ~ ((c("->") ~> term)) ^^ desugarLambda)    
  private def caseExpression = p("case" ~> c(term) ~ (c("of") ~> c("{")~> (branch+) <~ c("}")) ^^ CaseExpression)
  private def branch = p(pattern ~ (c("->") ~> c(term) <~ c(";")) ^^ Branch)  
  private def pattern = p(uident ~ (variable*) ^^ Pattern)
  
  def parseTerm(r: Reader[Char]) = strong(term) (new lexical.Scanner(r))  
  
  def typeDefinition: Parser[TypeDefinition] = p(typeConstrDefinition)  
  private def typeConstrDefinition = p(("data" ~> uident) ~ (typeVariable*) ~ ("=" ~> rep1sep(dataConstructor, "|") <~ ";") ^^
    TypeConstructorDefinition)
  
  private def tp1: Parser[Type] = p(uident ^^ {i => TypeConstructor(i, Nil)} | typeVariable | ("(" ~> `type` <~")"))
  private def tp2: Parser[Type] = p(typeVariable | uident ~ (tp1*) ^^ TypeConstructor)
  private def tp3: Parser[Type] = p(tp2 |  ("(" ~> `type` <~ ")"))
  private def `type` = {
    val c = {(x: Type, y: Type) => if (y == null) x else Arrow(x, y)}
    chainr1(tp3, "->" ^^^ c, c, null)
  }
  private def arrow: Parser[Arrow] = p(tp2 ~ ("->" ~> `type`) ^^ Arrow) | ("(" ~> arrow <~ ")") 
  private def typeVariable = p(lident ^^ TypeVariable)  
  private def dataConstructor = p(uident ~ (tp1*) ^^ {case n ~ a => DataConstructor(n, a)})
  
  def parseType(r: Reader[Char]) = strong(`type`) (new lexical.Scanner(r))
  def parseProgram(r: Reader[Char]) = postprocess(validate(strong(program) (new lexical.Scanner(r))))
  
  def validate(pr: ParseResult[Program]) = pr match {
    case n: NoSuccess => n;
    case s @ Success(_, _) => Validator0.validate(s)
  }
  
  def postprocess(pr: ParseResult[Program]) = pr match {
    case n: NoSuccess => n;
    case s @ Success(_, _) => Postprocessor0.postprocess(s.get); s
  }
  
  def p[T <: Positional](p: => Parser[T]): Parser[T] = positioned(p)
  
  def c[T](p: => Parser[T]): Parser[T] = commit(p)
  
  def desugarLambda(vs: List[Variable], e: Term): LambdaAbstraction = {
    def desugarLambda_(vs_ : List[Variable]) : Term = vs_ match {
      case Nil => e;
      case v :: vv => LambdaAbstraction(v, desugarLambda_(vv))
    }
    LambdaAbstraction(vs.head, desugarLambda_(vs.tail))
  }
  
  /* customized error: validation error */
  case class HError(override val msg: String, val pos: Positional) extends Error(msg, null) {
    override def toString = "[" + pos.pos +"] error: "+msg+"\n\n"+pos.pos.longString
  }
  
  def error(msg: String, pos: Positional) = {
    lastNoSuccess = null; val e = HError(msg, pos);  lastNoSuccess = null; e
  }
}