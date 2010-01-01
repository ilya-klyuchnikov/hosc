package hosc;

import scala.util.parsing.input.{Positional, Reader}
import scala.util.parsing.combinator.{ImplicitConversions, Parsers}
import HLanguage._

import scala.util.parsing.syntax.StdTokens
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers

object HParsers extends HTokenParsers with StrongParsers with ImplicitConversions {
  
  lexical.delimiters += ("(", ")", ",", "=", ";", "{", "}", "::", "|", "->", "\\")
  lexical.reserved += ("case", "of", "where", "data", "letrec", "in")
  
  def program = (typeConstrDefinition*) ~ term ~ (("where" ~> strong(function*)) | success(Nil)) ^^ Program
  def function = p(lident ~ ("=" ~> lambdaAbstraction <~ c(";")) ^^ Function)
  
  def term: Parser[Expression] = p(tr2 | appl) | err("term is expected")
  def appl = chainl1(tr0, tr1, success(Application(_: Expression, _: Expression)))
    
  // head of application
  private def tr0: Parser[Expression] = p(variable | lambdaAbstraction | caseExpression |("(" ~> appl <~ ")")) | letrec
  // argument of or application constructor
  private def tr1 = p(tr0 | uident ^^ {x => Constructor(x, Nil)} | ("(" ~> term <~ ")"))
  // top constructor; cannot be head of application
  private def tr2: Parser[Constructor] =  p(uident ~ (tr1*) ^^ Constructor | ("(" ~> tr2 <~ ")"))
  
  private def variable = p(lident ^^ Variable)  
  private def lambdaAbstraction:Parser[LambdaAbstraction] = p("\\" ~> c(variable+) ~ ((c("->") ~> term)) ^^ desugarLambda) | "(" ~> lambdaAbstraction <~ ")"
  private def caseExpression = p("case" ~> c(term) ~ (c("of") ~> c("{")~> (branch+) <~ c("}")) ^^ CaseExpression)
  private def letrec:Parser[LetRecExpression] = ("letrec" ~> c(variable)) ~ (c("=") ~> c(term)) ~ (c("in") ~> c(term)) ^^ 
                       {case v ~ l ~ e => LetRecExpression((v, l), e)} | ("(" ~> letrec <~ ")")
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
  
  def validate(pr: ParseResult[Program]): ParseResult[Program] = pr match {
    case n: NoSuccess => n;
    case s @ Success(_, _) => Validator.validate(s)
  }
  
  def postprocess(pr: ParseResult[Program]) = pr match {
    case n: NoSuccess => n;
    case s @ Success(_, _) => Postprocessor.postprocess(s.get); s
  }
  
  def p[T <: Positional](p: => Parser[T]): Parser[T] = positioned(p)
  
  def c[T](p: => Parser[T]): Parser[T] = commit(p)
  
  def desugarLambda(vs: List[Variable], e: Expression): LambdaAbstraction = {
    def desugarLambda_(vs_ : List[Variable]) : Expression = vs_ match {
      case Nil => e;
      case v :: vv => LambdaAbstraction(v, desugarLambda_(vv))
    }
    LambdaAbstraction(vs.head, desugarLambda_(vs.tail))
  }
  
  /* customized error: validation error */
  case class HError(override val msg: String, val pos: Positional) extends Error(msg, null) {
    override def toString = "[" + pos.pos +"] error: " + msg + "\n\n" + pos.pos.longString
  }
  
  def error(msg: String, pos: Positional) = {
    lastNoSuccess = null; val e = HError(msg, pos);  lastNoSuccess = null; e
  }
}

trait HTokens extends StdTokens {
  case class LIdentifier(chars: String) extends Token
  case class UIdentifier(chars: String) extends Token
}

import scala.util.parsing.input.CharArrayReader.EofCh
class HLexical extends StdLexical with HTokens {
  override def token: Parser[Token] = 
    (upperCaseLetter ~ rep(letter | digit) ^^ { case first ~ rest => processUIdent(first :: rest mkString "") }
    | lowerCaseLetter ~ rep(letter | digit) ^^ { case first ~ rest => processLIdent(first :: rest mkString "") }
    | super.token)

  protected def processLIdent(name: String) = if (reserved contains name) Keyword(name) else LIdentifier(name)
  protected def processUIdent(name: String) = if (reserved contains name) Keyword(name) else UIdentifier(name)
  def upperCaseLetter = elem("upper-case-letter", _.isUpperCase)
  def lowerCaseLetter = elem("lower-case-letter", _.isLowerCase)
  
    // see `whitespace in `Scanners'
  override def whitespace: Parser[Any] = rep(
      whitespaceChar
    | '/' ~ '*' ~ comment
    | '/' ~ '/' ~ rep( chrExcept(EofCh, '\n') )
    | '/' ~ '*' ~ failure("unclosed comment")
    
    | '{' ~ '-' ~ hComment
    | '-' ~ '-' ~ rep( chrExcept(EofCh, '\n') )
    | '{' ~ '-' ~ failure("unclosed comment")
    )

  override protected def comment: Parser[Any] = (
      '*' ~ '/'  ^^ { case _ => ' '  }
    | chrExcept(EofCh) ~ comment
    )
  
  protected def hComment: Parser[Any] = (
      '-' ~ '}'  ^^ { case _ => ' '  }
    | chrExcept(EofCh) ~ hComment
    )
}

class HTokenParsers extends StdTokenParsers {
  type Tokens = HTokens
  val lexical = new HLexical
  import lexical.{UIdentifier, LIdentifier}
  def uident: Parser[String] = elem("identifier", _.isInstanceOf[UIdentifier]) ^^ (_.chars)
  def lident: Parser[String] = elem("identifier", _.isInstanceOf[LIdentifier]) ^^ (_.chars)  
}

trait StrongParsers extends Parsers {
  
  // A parser generator that corresponds to p+~EOF
  // but instead of returning "EOF expected" at the middle of the file it reports where p has failed.
  def strongRep1[T](p: => Parser[T]): Parser[List[T]] = new Parser[List[T]]{
    def apply(in0: Input)  = {
      val xs = new scala.collection.mutable.ListBuffer[T]
      var in = in0
      var res: ParseResult[T] = null
      do {
        res = p(in)
        in = res.next
        if (res.successful) xs+=res.get
      } while (res.successful && !res.next.atEnd)
      res match {
        case s @ Success(out, in1) => Success(xs.toList, res.next)
        case f : NoSuccess => f
      }
    }
  }
  
  def strong[T](p: => Parser[T]): Parser[T] = new Parser[T]{
    def apply(in: Input)  = {
      val res = p(in)
      res match {
        case Success(out, in1) if res.next.atEnd  => res
        case Success(_, _) => Failure("EOF expected but " + res.next.first + " found", res.next)
        case f : NoSuccess => f
      }
    }
  }
}

object Postprocessor {
  def postprocess(program: Program) = {
    val globals = Set[Variable]() ++ (program.fs map (f => Variable(f.name)))
    for (f <- program.fs) process(f.lam,  globals)
    process(program.goal, globals)
  }
  def process(t: Expression, globals: Set[Variable]): Unit = t match {
    case v: Variable => v.global = (globals contains v)
    case Constructor(_, args) => for (a <- args) process(a, globals)
    case LambdaAbstraction(v, t) => process(t, globals)
    case Application(h, a) => process(h, globals); process(a, globals)
    case CaseExpression(s, bs) => process(s, globals); for (b <- bs) process(b.term, globals)
    case LetRecExpression((v, e), e0) => {v.global = true; process(e, globals + v); process(e0, globals + v)}                                                   
  }
}