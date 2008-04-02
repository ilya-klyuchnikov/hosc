package hosc

import scala.util.parsing.input.Reader
import HLanguage._

object HParsers extends HTokenParsers with StrongParsers {
  
  lexical.delimiters += ("(", ")", ",", "=", ";", ":", "%", "{", "}", "->", "::", "|")
  lexical.reserved += ("case", "of")
  
  // expressions
  def term = chainl1(aterm, success(Application(_: Term, _: Term)))
  
  private def aterm: Parser[Term] = variable | constructor | lambdaAbstraction | caseExpression |  ("(" ~> term <~ ")")  
  
  private def variable = lident ^^ Variable
  
  private def lambdaAbstraction =
    "%" ~> variable ~ ("{" ~> aterm <~ "}") ^^ {case v ~ t => LambdaAbstraction(v, t)}
    
  private def caseExpression = 
    "case" ~> aterm ~ ("of" ~> "{"~> (branch*) <~ "}") ^^ {case s ~ bs => CaseExpression(s, bs)}
  
  private def branch = 
    pattern ~ (":" ~> aterm <~ ";") ^^ {case p ~ t => Branch(p, t)}
  
  private def pattern =
    uident ~ (variable*) ^^ {case name ~ args => Pattern(name, args)}

  private def constructor =
    uident ~ (aterm*) ^^ {case name ~ args => Constructor(name, args)}
  
  def parseTerm(r: Reader[Char]) = strong(term, "term or <eof> expected") (new lexical.Scanner(r))
  
  def function = lident ~ ("=" ~> lambdaAbstraction) ^^ {case n ~ l => Function(n, l)}
  
  // types
  def typeDefinition: Parser[TypeDefinition] = typeConstructorDefinition | arrowDefinition
  
  private def typeConstructorDefinition = lident ~ (typeVariable*) ~ ("::" ~> rep1sep(dataConstructor, "|") <~ ";") ^^
    {case n ~ a ~ dc => TypeConstructorDefinition(n, a, dc)}
  
  private def arrowDefinition = lident ~ ("::" ~> arrow <~ ";") ^^ {case n ~ a => ArrowDefinition(n, a)}
  
  private def atype: Parser[Type] = typeConstructor | typeVariable | ("(" ~> `type` <~")")
  
  private def `type` = chainl1(atype, ("->" ~> atype), success(Arrow(_: Type, _: Type)))
  
  private def arrow = atype ~ ("->" ~> `type`) ^^ {case t1 ~ t2 => Arrow(t1, t2)}
  
  private def typeConstructor = lident ~ (atype*) ^^ {case n ~ a => TypeConstructor(n, a)}
  
  private def typeVariable = sident ^^ TypeVariable
  
  private def dataConstructor = uident ~ (atype*) ^^ {case n ~ a => DataConstructor(n, a)}
  
  // program itself
  def program = (typeDefinition*) ~ (function+) ^^ {case ts ~ fs => Program(ts, fs)}
  
  def parseProgram(r: Reader[Char]) = strong(program, "definition or <eof> expected") (new lexical.Scanner(r))
}
