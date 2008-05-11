package hosc

import scala.text.Document
import scala.text.Document._

/** 
 * used by residual program generator
 * deprecated when full syntax is complete
 */
object HLanguage1 {
   val ED: scala.text.Document = empty
  
   sealed abstract class Expression1 {
     def toDoc: Document
   }
   
   case class Variable1(name: String) extends Expression1 {
     var global = false // global var is call
     override def toString = name
     def toDoc = text(name)
   }
   
   case class Constructor1(name: String, args: List[Expression1]) extends Expression1 {
     override def toString = "(" + name + " " + args.mkString(" ") + ")";     
     def toDoc = args match {
       case Nil => text(name)
       case _ => group(("(" + name) :: nest(2, ED :: args.foldRight(ED){(x, y) => ED :/: x.toDoc :: y}) :: ")" :: ED)
     }
   }
   
   case class LambdaAbstraction1(v: Variable1, t: Expression1) extends Expression1 {
     override def toString = "%" + v.name + " {" + t + "}";
     def toDoc = "%" :: v.toDoc :: " {" :: nest(2, ED :/: t.toDoc) :/: "}" :: ED 
   }
   
   case class Application1(head: Expression1, arg: Expression1) extends Expression1 {
     override def toString = "(" + head + " " + arg + ")";
     def toDoc = group("(" :: nest(2, head.toDoc :/: arg.toDoc ) :: ")" :: ED)
   }
   
   case class CaseExpression1(selector: Expression1, branches: List[Branch1]) extends Expression1 {
     override def toString = "case (" + selector + ") of " + branches.mkString("{", " ", "}")
     def toDoc = group( group("case " :/: selector.toDoc :/: " of {" :: ED) :: 
       nest(2, branches.foldRight(ED){(b, y) => ED :/: b.toDoc :: y}) :/: "}" :: ED)
   }
   
   case class LetExpression1(bs: List[Pair[Variable1, Expression1]], expr: Expression1) extends Expression1 {
     override def toString = "let " + (bs map {p => p._1 + "=" + p._2}).mkString(", ") + " in " + expr;
     def toDoc = group("let" :: 
         nest(2, ED :/: bs.foldRight(ED){(b, y) => group (b._1.toDoc :: " = " :: b._2.toDoc) :/: y})
         :/: "in " :: nest(2, ED :/: expr.toDoc))
   }
   
   case class LetRecExpression1(bs: List[Pair[Variable1, Expression1]], expr: Expression1) extends Expression1 {
     override def toString = "letrec " + (bs map {p => p._1 + "=" + p._2}).mkString(", ") + " in " + expr;
     def toDoc = group("letrec" :: 
         nest(2, bs.foldRight(ED){(b, y) => group (ED :/: b._1.toDoc :: " = " :: b._2.toDoc) :: y})
         :/: "in " :: nest(2, ED :/: expr.toDoc))
   }
   
   case class Branch1(pattern: Pattern1, term: Expression1) {
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
   
}
