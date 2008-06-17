package hosc.h1parsers;

import scala.util.parsing.combinator.lexical.StdLexical

class HLexical1 extends StdLexical with HTokens1 {
  override def token: Parser[Token] = 
    ( '$' ~ rep1( letter | digit ) ^^ { case first ~ rest => processSIdent(first :: rest mkString "") }
    | letter ~ '$' ~ rep1( letter | digit ) ^^ { case first0  ~ first ~ rest => processFIdent(first0 :: first :: rest mkString "") }
    | upperCaseLetter ~ rep(letter | digit) ^^ { case first ~ rest => processUIdent(first :: rest mkString "") }
    | lowerCaseLetter ~ rep(letter | digit) ^^ { case first ~ rest => processLIdent(first :: rest mkString "") }
    | super.token
    )
    
  protected def processSIdent(name: String) = 
    if (reserved contains name) Keyword(name) else SIdentifier(name)
    
  protected def processLIdent(name: String) = 
    if (reserved contains name) Keyword(name) else LIdentifier(name)
    
  protected def processUIdent(name: String) = 
    if (reserved contains name) Keyword(name) else UIdentifier(name)
    
  protected def processFIdent(name: String) = 
    if (reserved contains name) Keyword(name) else FIdentifier(name)
    
  def upperCaseLetter = elem("upper-case-letter", _.isUpperCase)
  def lowerCaseLetter = elem("lower-case-letter", _.isLowerCase)
}
