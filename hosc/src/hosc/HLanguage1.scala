package hosc

import scala.text.Document
import scala.text.Document._

object HLanguage1 {
   val ED: scala.text.Document = empty
   sealed abstract class Label 
   case class Loop extends Label
   case class Repeat extends Label
   
   sealed abstract class Expression1 {
     def toDoc: Document
   }
   sealed abstract class Term1 extends Expression1 {
     var label : Label = null
     def labelToString = label match {
       case Loop() => "Loop: "
       case Repeat() => "Repeat: "
       case null => ""
     }
   }
   
   case class Variable1(name: String) extends Term1 {
     var call = false
     override def toString = labelToString + name
     def toDoc = text(name)
   }
   
   case class Constructor1(name: String, args: List[Term1]) extends Term1 {
     override def toString = labelToString + "(" + name + " " + args.mkString(" ") + ")";     
     def toDoc = args match {
       case Nil => text(name)
       case _ => group(("(" + name) :: nest(2, ED :: args.foldRight(ED){(x, y) => ED :/: x.toDoc :: y}) :: ")" :: ED)
     }
   }
   
   case class LambdaAbstraction1(v: Variable1, t: Term1) extends Term1 {
     override def toString = labelToString + "%" + v.name + " {" + t + "}";
     def toDoc = "%" :: v.toDoc :: " {" :: nest(2, ED :/: t.toDoc) :/: "}" :: ED 
   }
   
   case class Application1(head: Term1, arg: Term1) extends Term1 {
     override def toString = labelToString + "(" + head + " " + arg + ")";
     def toDoc = group("(" :: nest(2, head.toDoc :/: arg.toDoc ) :: ")" :: ED)
   }
   
   case class CaseExpression1(selector: Term1, branches: List[Branch1]) extends Term1 {
     override def toString = labelToString + "case (" + selector + ") of " + branches.mkString("{", " ", "}")
     def toDoc = group( group("case " :/: selector.toDoc :/: " of {" :: ED) :: 
       nest(2, branches.foldRight(ED){(b, y) => ED :/: b.toDoc :: y}) :/: "}" :: ED)
   }
   
   case class LetRecExpression1(binding: Pair[Variable1, Term1], expr: Term1) extends Term1 {
     override def toString = labelToString + "letrec " + (binding._1 + "=" + binding._2) + " in " + expr;
     def toDoc = group("letrec" :: 
         nest(2, group (ED :/: binding._1.toDoc :: " = " :: binding._2.toDoc))
         :/: "in " :: nest(2, ED :/: expr.toDoc))
   }
   
   case class LetExpression1(bs: List[Pair[Variable1, Term1]], expr: Term1) extends Expression1 {
     override def toString = "let " + (bs map {p => p._1 + "=" + p._2}).mkString(", ") + " in " + expr;
     def toDoc = group("let" :: 
         nest(2, ED :/: bs.foldRight(ED){(b, y) => group (b._1.toDoc :: " = " :: b._2.toDoc) :/: y})
         :/: "in " :: nest(2, ED :/: expr.toDoc))
   }
   
   case class Branch1(pattern: Pattern1, term: Term1) {
     override def toString = pattern + " : " + term + ";"
     def toDoc: Document = group(pattern.toDoc :: " :" :: nest(2 , ED :/: term.toDoc :: ";" :: ED)); 
   }
   
   case class Pattern1(name: String, args: List[Variable1]) {
     override def toString = args match {
       case Nil => name
       case _ => name + " " + args.mkString(" ")
     }
     def toDoc = text(toString)
   }
   
   case class Program1(ts: List[TypeDefinition], expr: Term1) {
     def toDoc = (ED /: ts) {(doc, td) => doc :/: nest(0, text(td.toString))} :/: nest(0, expr.toDoc)
   }
   
}
