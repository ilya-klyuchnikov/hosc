package hosc;

import scala.util.parsing.input.Positional
import scala.text.Document
import scala.text.Document._

/** 
 * Abstract syntax for simple higher-order language
 */
object HLanguage {
   val ED: scala.text.Document = empty
   sealed abstract class Expression extends Positional {
     def toDoc: Document
   }
   sealed abstract class BaseExpression extends Expression
   
   sealed abstract class Term extends BaseExpression
   case class Variable(name: String) extends Term {
     var global = false // global var is call
     override def toString = name
     def toDoc = text(name)
   } 
   case class Constructor(name: String, args: List[Term]) extends Term {
     override def toString = "(" + name + " " + args.mkString(" ") + ")"
     def toDoc = args match {
       case Nil => text(name)
       case _ => group(("(" + name) :: nest(2, ED :: args.foldRight(ED){(x, y) => ED :/: x.toDoc :: y}) :: ")" :: ED)
     }  
   }
   case class LambdaAbstraction(v: Variable, t: Term) extends Term {
     override def toString = "%" + v.name + " {" + t + "}" 
     def toDoc = "%" :: v.toDoc :: " {" :: nest(2, ED :/: t.toDoc) :/: "}" :: ED 
   }
   case class Application(head: Term, arg: Term) extends Term {
     pos = head.pos
     override def toString = "(" + head + " " + arg + ")"
     def toDoc = group("(" :: nest(2, head.toDoc :/: arg.toDoc ) :: ")" :: ED)
   } 
   case class CaseExpression(selector: Term, branches: List[Branch]) extends Term {
     override def toString = "case (" + selector + ") of " + branches.mkString("{", " ", "}")
     def toDoc = group( group("case " :/: selector.toDoc :/: " of {" :: ED) :: 
       nest(2, branches.foldRight(ED){(b, y) => ED :/: b.toDoc :: y}) :/: "}" :: ED)
   }
   case class LetExpression(bs: List[Pair[Variable, Expression]], expr: Expression) extends BaseExpression {
     override def toString = "(let " + (bs map {p => p._1 + "=" + p._2}).mkString(", ") + "\n in " + expr + ")" 
     def toDoc = group("let" :: 
           nest(2, ED :/: bs.foldRight(ED){(b, y) => group (b._1.toDoc :: " = " :: b._2.toDoc) :/: y})
           :/: "in " :: nest(2, ED :/: expr.toDoc))
   }
   case class LetRecExpression(bs: List[Pair[Variable, Expression]], expr: Expression) extends Expression {
     override def toString = "(letrec " + (bs map {p => p._1 + "=" + p._2}).mkString(", ") + "\n in " + expr + ")"
     def toDoc = group("letrec" :: 
           nest(2, ED :/: bs.foldRight(ED){(b, y) => group (b._1.toDoc :: " = " :: b._2.toDoc) :/: y})
           :/: "in " :: nest(2, ED :/: expr.toDoc))
   }
   
   case class Branch(pattern: Pattern, term: Term) extends Positional {
     override def toString = pattern + " : " + term + ";"
     def toDoc: Document = group(pattern.toDoc :: " :" :: nest(2 , ED :/: term.toDoc :: ";" :: ED)); 
   }
   case class Pattern(name: String, args: List[Variable]) extends Positional {
     override def toString = name + " " + args.mkString(" ")
     def toDoc = text(toString)
   }
   
   case class Function(name: String, lam: LambdaAbstraction) extends Positional {
     def toDoc = text(name) :: text(" = ") :: lam.toDoc
   }
   
   case class Program(ts: List[TypeDefinition], goal: Term, fs: List[Function]) {
     def getTypeConstructorDefinition(tcName: String): Option[TypeConstructorDefinition] = {
       for (td <- ts) td match {case tcd: TypeConstructorDefinition if tcName == tcd.name => return Some(tcd); case _ => } 
       None
     }
     def getTypeDefinitionForDC(dataConsName: String): Option[TypeConstructorDefinition] = {
       for (td <- ts) td match {
         case tcd: TypeConstructorDefinition => for (dc <- tcd.cons) if (dc.name == dataConsName) return Some(tcd)
         case _ => 
       }
       None
     }
     def getDataConstructor(dataConsName: String): Option[DataConstructor] = {
       for (td <- ts) td match {
         case tcd: TypeConstructorDefinition => for (dc <- tcd.cons) if (dc.name == dataConsName) return Some(dc)
         case _ => 
       }
       None
     }
     def getFunction(n: String): Option[Function] = {
       for (f <- fs) if (f.name == n) return Some(f)
       None
     }
     def toDoc = (ED /: ts) {(doc, td) => doc :/: nest(0, text(td.toString))} :/: 
                 ED :/: nest(0, goal.toDoc :/: text("where")) :/:
                 (ED /: fs) {(doc, f) => doc :/: nest(0, f.toDoc)}
   }
}
