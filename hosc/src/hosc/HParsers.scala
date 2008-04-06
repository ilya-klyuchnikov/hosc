package hosc

import scala.util.parsing.input.Reader
import scala.util.parsing.input.Positional
import HLanguage._

object HParsers extends HTokenParsers with StrongParsers {
  
  lexical.delimiters += ("(", ")", ",", "=", ";", ":", "%", "{", "}", "->", "::", "|")
  lexical.reserved += ("case", "of")
  
  // expressions
  def term = chainl1(aterm, success(Application(_: Term, _: Term)))
  
  private def aterm: Parser[Term] = p(variable | constructor | lambdaAbstraction | caseExpression |  ("(" ~> term <~ ")"))  
  
  private def variable = p(lident ^^ Variable)
  
  private def lambdaAbstraction =
    p("%" ~> variable ~ ("{" ~> term <~ "}") ^^ {case v ~ t => LambdaAbstraction(v, t)})
    
  private def caseExpression = 
    p("case" ~> term ~ ("of" ~> "{"~> (branch*) <~ "}") ^^ {case s ~ bs => CaseExpression(s, bs)})
  
  private def branch = 
    p(pattern ~ (":" ~> term <~ ";") ^^ {case p ~ t => Branch(p, t)})
  
  private def pattern =
    p(uident ~ (variable*) ^^ {case name ~ args => Pattern(name, args)})

  private def constructor =
    p(uident ~ (aterm*) ^^ {case name ~ args => Constructor(name, args)})
  
  def parseTerm(r: Reader[Char]) = strong(term, "term or <eof> expected") (new lexical.Scanner(r))
  
  def function = p(lident ~ ("=" ~> lambdaAbstraction) ^^ {case n ~ l => Function(n, l)})
  
  private def typeDefinition = p(lident ~ (typeVariable*) ~ ("::" ~> rep1sep(dataConstructor, "|") <~ ";") ^^
    {case n ~ a ~ dc => TypeDefinition(n, a, dc)})
  
  private def atype: Parser[Type] = p(typeConstructor | typeVariable | ("(" ~> `type` <~")"))
  
  private def `type` = chainl1(atype, ("->" ~> atype), success(Arrow(_: Type, _: Type)))
  
  private def arrow = p(atype ~ ("->" ~> `type`) ^^ {case t1 ~ t2 => Arrow(t1, t2)})
  
  private def typeConstructor = lident ~ (atype*) ^^ {case n ~ a => TypeConstructor(n, a)}
  
  private def typeVariable = sident ^^ TypeVariable
  
  private def dataConstructor = uident ~ (atype*) ^^ {case n ~ a => DataConstructor(n, a)}
  
  // program itself
  def program = (typeDefinition*) ~ (function+) ^^ {case ts ~ fs => Program(ts, fs)}
  
  def parseProgram(r: Reader[Char]) = validate(strong(program, "definition or <eof> expected") (new lexical.Scanner(r)))
  
  def p[T <: Positional](p: => Parser[T]): Parser[T] = positioned(p)
  
  // for representation of error encountered at second-phase checking
  case class HError(override val msg: String, val pos: Positional) extends Error(msg, null) {
    override def toString = "[" + pos+"] error: "+msg+"\n\n"+pos.pos.longString
  }
  
  def validate(pr: ParseResult[Program]) = pr match {
    case f: NoSuccess => f
    case Success(p, _) => pr
  }
}
