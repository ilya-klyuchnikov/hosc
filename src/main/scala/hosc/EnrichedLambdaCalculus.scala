package hosc

import scala.text.Document
import scala.text.Document._

object EnrichedLambdaCalculus {
   val ED: scala.text.Document = empty
   sealed abstract class Expression {
     def toDoc: Document
     def toDocString = {    
       val writer1 = new java.io.StringWriter()
       toDoc.format(150, writer1)
       writer1.toString
     }
   }
   case class Variable(name: String) extends Expression {
     override def toString = name
     def toDoc = text(name)
   } 
   case class Constructor(name: String, args: List[Expression]) extends Expression {
     override def toString = "(" + name + " " + args.mkString(" ") + ")"
     def toDoc = args match {
       case Nil => text(name)
       case _ => group(("(" + name) :: nest(2, ED :: args.foldRight(ED){(x, y) => ED :/: x.toDoc :: y}) :: ")" :: ED)
     }  
   }
   case class LambdaAbstraction(v: Variable, t: Expression) extends Expression {
     override def toString = "\\" + v.name + " -> (" + t + ")" 
     def toDoc = "\\" :: v.toDoc :: " -> (" :: nest(2, ED :/: t.toDoc) :/: ")" :: ED 
   }
   case class Application(head: Expression, arg: Expression) extends Expression {
     override def toString = "(" + head + " " + arg + ")"
     def toDoc = group("(" :: nest(2, head.toDoc :/: arg.toDoc ) :: ")" :: ED)
   } 
   case class CaseExpression(selector: Expression, branches: List[Branch]) extends Expression {
     override def toString = "case (" + selector + ") of " + branches.mkString("{", " ", "}")
     def toDoc = group( group("case " :/: selector.toDoc :/: " of {" :: ED) :: 
       nest(2, branches.foldRight(ED){(b, y) => ED :/: b.toDoc :: y}) :/: "}" :: ED)
   }
   case class LetExpression(bs: List[(Variable, Expression)], expr: Expression) extends Expression {
     override def toString = "let " + (bs map {p => p._1 + "=" + p._2}).mkString(", ") + "\n in " + expr 
     def toDoc = group("let" :: 
           nest(2, bs.foldLeft(ED){(y, b) => y :/: group (b._1.toDoc :: " = " :: b._2.toDoc)})
           :/: "in " :: nest(2, ED :/: expr.toDoc))
   }
   case class LetRecExpression(bs: List[(Variable, Expression)], expr: Expression) extends Expression {
     override def toString = "(letrec " + (bs map {p => p._1 + "=" + p._2}).mkString(", ") + "\n in " + expr + ")"
     def toDoc = group("letrec" :: 
           nest(2, bs.foldLeft(ED){(y, b) => y :/: group (b._1.toDoc :: " = " :: b._2.toDoc)})
           :/: "in " :: nest(2, ED :/: expr.toDoc))
   }   
   case class Branch(pattern: Pattern, term: Expression) {
     override def toString = pattern + " -> " + term + ";"
     def toDoc = group(pattern.toDoc :: " ->" :: nest(2 , ED :/: term.toDoc :: ";" :: ED)); 
   }
   case class Pattern(name: String, args: List[Variable]) {
     override def toString = name + " " + args.mkString(" ")
     def toDoc = text(toString)
   }
}