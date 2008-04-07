package hosc

import scala.util.parsing.input.{Positional, Reader}
import HLanguage._

object HParsers extends HTokenParsers with StrongParsers {
  
  lexical.delimiters += ("(", ")", ",", "=", ";", ":", "%", "{", "}", "->", "::", "|")
  lexical.reserved += ("case", "of")

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
  
  def typeDefinition: Parser[TypeDefinition] = p(typeConstrDefinition | arrowDefinition)
  
  private def typeConstrDefinition = p(lident ~ (typeVariable*) ~ ("::" ~> rep1sep(dataConstructor, "|") <~ ";") ^^
    {case n ~ a ~ dc => TypeConstructorDefinition(n, a, dc)})
    
  private def arrowDefinition = p(lident ~ ("::" ~> arrow <~ ";") ^^ {case n ~ a => ArrowDefinition(n, a)})
  
  private def t1: Parser[Type] = lident ^^ {i => TypeConstructor(i, Nil)} | typeVariable | ("(" ~> `type` <~")")
  private def t2: Parser[Type] = typeVariable | lident ~ (t1*) ^^ {case i ~ args => TypeConstructor(i, args)}  
  private def `type` = chainl1(t2, ("->" ~> t2), success(Arrow(_: Type, _: Type)))  
  private def arrow = p(t2 ~ ("->" ~> `type`) ^^ {case t1 ~ t2 => Arrow(t1, t2)})  
  private def typeVariable = p(sident ^^ TypeVariable)  
  private def dataConstructor = p(uident ~ (`type`*) ^^ {case n ~ a => DataConstructor(n, a)})
  
  def program = (typeDefinition*) ~ (function+) ^^ {case ts ~ fs => Program(ts, fs)}
  
  def parseProgram(r: Reader[Char]) = validate(strong(program, "definition or <eof> expected") (new lexical.Scanner(r)))
  
  def validate(pr: ParseResult[Program]) = pr match {
    case n: NoSuccess => n;
    case s @ Success(_, _) => Validator.validate(s)
  }
  
  def p[T <: Positional](p: => Parser[T]): Parser[T] = positioned(p)
  
  def error(msg: String, pos: Positional) = {
    lastNoSuccess = null; val e = HError(msg, pos);  lastNoSuccess = null; e
  }
  
  case class HError(override val msg: String, val pos: Positional) extends Error(msg, null) {
    override def toString = "[" + pos.pos +"] error: "+msg+"\n\n"+pos.pos.longString
  }
}
