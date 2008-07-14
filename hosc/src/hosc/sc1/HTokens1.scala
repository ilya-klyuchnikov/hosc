package hosc.sc1

import scala.util.parsing.syntax.StdTokens

trait HTokens1 extends StdTokens {
  case class SIdentifier(chars: String) extends Token {
    override def toString = "sidentifier "+chars
  }
  
  case class LIdentifier(chars: String) extends Token {
    override def toString = "lidentifier "+chars
  }
  
  case class UIdentifier(chars: String) extends Token {
    override def toString = "uidentifier "+chars
  }
  
  case class FIdentifier(chars: String) extends Token {
    override def toString = "uidentifier "+chars
  }
}
