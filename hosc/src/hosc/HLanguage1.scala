package hosc

import scala.util.parsing.input.Positional
import scala.text.Document
import scala.text.Document._

object HLanguage1 {
   val ED: scala.text.Document = empty
   sealed abstract class Label 
   case class Loop() extends Label
   case class Repeat() extends Label
   
   sealed abstract class Expression1 extends Positional {
     def toDoc: Document
     def isLoop: Boolean
     def \\(s: Map[Variable1, Variable1]): Expression1
   }
   sealed abstract class Term1 extends Expression1 {
     type termType <: Term1
     // replace all vars including bound ones
     def \\(s: Map[Variable1, Variable1]): termType
     var label : Label = null
     def labelToString = label match {
       case Loop() => "Loop: "
       case Repeat() => "Repeat: "
       case null => ""
     }
     def isLoop = label == Loop()
     def / (s: Map[Variable1, Term1]): Term1
   }
   
   case class Variable1(name: String) extends Term1 {
     type termType = Variable1
     def  \\(s: Map[Variable1, Variable1]) = s.get(this) match {case Some(t) => t; case None => this}
     def / (s: Map[Variable1, Term1]) = s.get(this) match {case Some(t) => t; case None => this}
     var call = false // variable is defined by letrec construction or is defined in original program
     override def toString = labelToString + (if (call) "[" + name+ "]" else name)
     def toDoc = text(name)
   }
   
   case class Constructor1(name: String, args: List[Term1]) extends Term1 {
     type termType = Constructor1
     def \\(s: Map[Variable1, Variable1]) ={val t=Constructor1(name, args map {_\\s}); t.label = label; t}
     def / (s: Map[Variable1, Term1]) =  {val t=Constructor1(name, args map {_/s}); t.label = label; t}
     override def toString = labelToString + "(" + name + " " + args.mkString(" ") + ")";     
     def toDoc = args match {
       case Nil => text(name)
       case _ => group(("(" + name) :: nest(2, ED :: args.foldRight(ED){(x, y) => ED :/: x.toDoc :: y}) :: ")" :: ED)
     }
   }
   
   case class LambdaAbstraction1(v: Variable1, t: Term1) extends Term1 {
     type termType = LambdaAbstraction1
     def \\(s: Map[Variable1, Variable1]) = {val l1=LambdaAbstraction1(v\\s, t\\s);l1.label=label;l1}
     def / (s: Map[Variable1, Term1]) = {val l1=LambdaAbstraction1(v, t/s);l1.label=label;l1}
     override def toString = labelToString + "(\\" + v.name + " ->" + t + ")";
     def toDoc = "(\\" :: v.toDoc :: " -> " :: t.toDoc :: ")" :: ED 
   }
   
   case class Application1(head: Term1, arg: Term1) extends Term1 {
     type termType = Application1
     def \\(s: Map[Variable1, Variable1]) = {val a1=Application1(head\\s, arg\\s);a1.label=label;a1}
     def / (s: Map[Variable1, Term1]) = {val a1=Application1(head/s, arg/s);a1.label=label;a1}
     override def toString = labelToString + "(" + head + " " + arg + ")";
     def toDoc = group("(" :: nest(2, head.toDoc :/: arg.toDoc ) :: ")" :: ED)
   }
   
   case class CaseExpression1(selector: Term1, branches: List[Branch1]) extends Term1 {
     type termType = CaseExpression1
     def \\(s: Map[Variable1, Variable1]) = {val c1=CaseExpression1(selector\\s, branches map {_\\s});c1.label=label;c1}
     def / (s: Map[Variable1, Term1]) = {
       val c1=CaseExpression1(selector/s, branches map {b => Branch1(b.pattern, b.term / s)});
       c1.label=label;c1
     }
     override def toString = labelToString + "case (" + selector + ") of " + branches.mkString("{", " ", "}")
     def toDoc = group( group("case" :/: selector.toDoc :/: "of {" :: ED) :: 
       nest(2, branches.foldRight(ED){(b, y) => ED :/: b.toDoc :: y}) :/: "}" :: ED)
   }
   
   case class LetRecExpression1(binding: (Variable1, Term1), expr: Term1) extends Term1 {
     type termType = LetRecExpression1
     def \\(s: Map[Variable1, Variable1]) = {
       val l1=LetRecExpression1((binding._1\\s, binding._2\\s), expr\\s);
       l1.label=label;l1
     }
     def / (s: Map[Variable1, Term1]) = {
       val l1=LetRecExpression1((binding._1, binding._2/s), expr/s);
       l1.label=label;l1
     }
     override def toString = labelToString + "letrec " + (binding._1 + "=" + binding._2) + " in " + expr;
     def toDoc = group("(letrec" :: 
         nest(2, group (ED :/: binding._1.toDoc :: "=" :: binding._2.toDoc))
         :/: "in" :: nest(2, ED :/: expr.toDoc) :: ")" :: ED)
   }
   
   case class LetExpression1(bs: List[(Variable1, Term1)], expr: Term1) extends Expression1 {
     def \\(s: Map[Variable1, Variable1]) = LetExpression1(bs map {b => ((b._1\\s).asInstanceOf[Variable1],
         (b._2\\s).asInstanceOf[Term1])}, expr\\s);
     def isLoop = false
     override def toString = "let " + (bs map {p => p._1 + "=" + p._2}).mkString(", ") + " in " + expr;
     def toDoc = group("let" :: 
         nest(2, ED :/: bs.foldRight(ED){(b, y) => group (b._1.toDoc :: " = " :: b._2.toDoc) :/: y})
         :/: "in " :: nest(2, ED :/: expr.toDoc))
   }
   
   case class Branch1(pattern: Pattern1, term: Term1) {
     def \\(s: Map[Variable1, Variable1]) = Branch1(pattern\\s, term\\s)
     override def toString = pattern + " -> " + term + ";"
     def toDoc: Document = group(pattern.toDoc :: " ->" :: nest(2 , ED :/: term.toDoc :: ";" :: ED)); 
   }
   
   case class Pattern1(name: String, args: List[Variable1]) {
     def \\(s: Map[Variable1, Variable1]) = Pattern1(name, args map {_\\s})
     override def toString = args match {
       case Nil => name
       case _ => name + " " + args.mkString(" ")
     }
     def toDoc = text(toString)
   }
   
   case class Program1(ts: List[TypeConstructorDefinition], expr: Term1) {
     def toDoc = (ED /: ts) {(doc, td) => doc :/: nest(0, text(td.toString))} :/: nest(0, expr.toDoc)
   }
   
}
