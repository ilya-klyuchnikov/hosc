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
     type termType <: Expression
     def \\(s: Map[Variable, Variable]): termType
     def / (s: Map[Variable, Expression]): Expression
   }
   case class Variable(name: String) extends Expression {
     type termType = Variable
     def  \\(s: Map[Variable, Variable]) = s.get(this) match {case Some(t) => t; case None => this}
     def / (s: Map[Variable, Expression]) = s.get(this) match {case Some(t) => t; case None => this}
     var global = false // global var is call
     override def toString = name
     def toDoc = text(name)
   } 
   case class Constructor(name: String, args: List[Expression]) extends Expression {
     type termType = Constructor
     def \\(s: Map[Variable, Variable]) = Constructor(name, args map {_\\s})
     def / (s: Map[Variable, Expression]) = Constructor(name, args map {_/s})
     override def toString = "(" + name + " " + args.mkString(" ") + ")"
     def toDoc = args match {
       case Nil => text(name)
       case _ => group(("(" + name) :: nest(2, ED :: args.foldRight(ED){(x, y) => ED :/: x.toDoc :: y}) :: ")" :: ED)
     }  
   }
   case class LambdaAbstraction(v: Variable, t: Expression) extends Expression {
     type termType = LambdaAbstraction
     def \\(s: Map[Variable, Variable]) = LambdaAbstraction(v\\s, t\\s)
     def / (s: Map[Variable, Expression]) = LambdaAbstraction(v, t/s)
     override def toString = "(\\" + v.name + " -> " + t + ")"
     def toDoc = "(\\" :: v.toDoc :: "->" :: nest(2, ED :/: t.toDoc) :: ")" :: ED 
   }
   case class Application(head: Expression, arg: Expression) extends Expression {
     type termType = Application
     def \\(s: Map[Variable, Variable]) = Application(head\\s, arg\\s)
     def / (s: Map[Variable, Expression]) = Application(head/s, arg/s)
     pos = head.pos
     override def toString = "(" + head + " " + arg + ")"
     def toDoc = group("(" :: nest(2, head.toDoc :/: arg.toDoc ) :: ")" :: ED)
   } 
   case class CaseExpression(selector: Expression, branches: List[Branch]) extends Expression {
     type termType = CaseExpression
     def \\(s: Map[Variable, Variable]) = CaseExpression(selector\\s, branches map {_\\s})
     def / (s: Map[Variable, Expression]) = CaseExpression(selector/s, branches map {b => Branch(b.pattern, b.term / s)})
     override def toString = "case (" + selector + ") of " + branches.mkString("{", " ", "}")
     def toDoc = group( group("case " :/: selector.toDoc :/: " of {" :: ED) :: 
       nest(2, branches.foldRight(ED){(b, y) => ED :/: b.toDoc :: y}) :/: "}" :: ED)
   }
   case class LetExpression(bs: List[(Variable, Expression)], expr: Expression) extends Expression {
     type termType = LetExpression
     // we need asInstanceOf due to http://lampsvn.epfl.ch/trac/scala/ticket/252
     def \\(s: Map[Variable, Variable]) = LetExpression(bs map {b => ((b._1\\s).asInstanceOf[Variable], (b._2\\s).asInstanceOf[Expression])}, expr\\s);
     def / (s: Map[Variable, Expression]) = LetExpression(bs map {b => (b._1, (b._2/s).asInstanceOf[Expression])}, expr/s)
     override def toString = "let " + (bs map {p => p._1 + "=" + p._2}).mkString(", ") + "\n in " + expr 
     def toDoc = group("let" :: 
           nest(2, bs.foldLeft(ED){(y, b) => y :/: group (b._1.toDoc :: " = " :: b._2.toDoc)})
           :/: "in " :: nest(2, ED :/: expr.toDoc))
   }
   case class LetRecExpression(binding: (Variable, Expression), expr: Expression) extends Expression {
     type termType = LetRecExpression
     def \\(s: Map[Variable, Variable]) = LetRecExpression((binding._1\\s, binding._2\\s), expr\\s);
     def / (s: Map[Variable, Expression]) = LetRecExpression((binding._1, binding._2/s), expr/s)
     override def toString = "(letrec " + (binding._1 + "=" + binding._2) + "\n in " + expr + ")"
     def toDoc = group("(letrec" :: 
         nest(2, group (ED :/: binding._1.toDoc :: "=" :: binding._2.toDoc))
         :/: "in" :: nest(2, ED :/: expr.toDoc) :: ")" :: ED)
   }
   case class Choice(e1: Expression, e2: Expression) extends Expression {
     type termType = Expression
     def \\(s: Map[Variable, Variable]) = Choice(e1\\s, e2\\s)
     def /(s: Map[Variable, Expression]) = Choice(e1/s, e2/s)
     override def toString = "choice{" + e1 + "; " + e2 + ";}"
     def toDoc = group("choice{" :: nest(2, e1.toDoc :/: ";" :/: e2.toDoc :: ";" :: ED) :: "}" :: ED)
   }
   
   case class Branch(pattern: Pattern, term: Expression) extends Positional {
     def \\(s: Map[Variable, Variable]) = Branch(pattern\\s, term\\s)
     override def toString = pattern + " -> " + term + ";"
     def toDoc: Document = group(pattern.toDoc :: " ->" :: nest(2 , ED :/: term.toDoc :: ";" :: ED)); 
   }
   case class Pattern(name: String, args: List[Variable]) extends Positional {
     def \\(s: Map[Variable, Variable]) = Pattern(name, args map {_\\s})
     override def toString = name + " " + args.mkString(" ")
     def toDoc = text(toString)
   }
   
   case class Function(name: String, lam: LambdaAbstraction) extends Positional {
     def toDoc = text(name) :: text(" = ") :: lam.toDoc :: ";" :: ED
   }
   
   case class Program(ts: List[TypeConstructorDefinition], goal: Expression, fs: List[Function]) {
     def getTypeConstructorDefinition(tcName: String): Option[TypeConstructorDefinition] = {
       for (td <- ts) td match {case tcd: TypeConstructorDefinition if tcName == tcd.name => return Some(tcd); case _ => } 
       None
     }
     def getTypeDefinitionForDC(dataConsName: String): Option[TypeConstructorDefinition] = {
       for (td <- ts) 
         for (dc <- td.cons) 
           if (dc.name == dataConsName) return Some(td)
       None
     }
     def getDataConstructor(dataConsName: String): Option[DataConstructor] = {
       for (td <- ts)
         for (dc <- td.cons) 
           if (dc.name == dataConsName) return Some(dc)
       None
     }
     def getFunction(n: String): Option[Function] = {
       for (f <- fs) if (f.name == n) return Some(f)
       None
     }
     def toDoc = (ED /: ts) {(doc, td) => doc :/: nest(0, text(td.toString))} :/: 
                 ED :/: nest(0, goal.toDoc) :/: 
                 (if (fs.isEmpty) ED else text("where") :/: (ED /: fs) {(doc, f) => doc :/: nest(0, f.toDoc)})
     
     def toDocString = {
       val doc1 = toDoc    
       val writer1 = new java.io.StringWriter()
       doc1.format(120, writer1)
       writer1.toString
     }
   }
}

sealed abstract class Type extends Positional
case class TypeVariable(name: String) extends Type {
  override def toString = name
}
case class TypeConstructor(name: String, typeParameters: List[Type]) extends Type {
  override def toString = typeParameters match {
    case Nil => name
    case _   => "(" + name + " " + typeParameters.mkString(" ") + ")";
  }
}
case class Arrow(t1: Type, t2: Type) extends Type {
  pos = t1.pos
  override def toString = "(" + t1 + "->" + t2 + ")"
}
 
abstract sealed class TypeDefinition extends Positional {
  def name: String
}
case class TypeConstructorDefinition(name: String, args: List[TypeVariable], cons: List[DataConstructor])
  extends TypeDefinition {
  override def toString = "data " + name + " " + args.mkString(" ") + " = " + cons.mkString(" | ") + ";";     
}
case class ArrowDefinition(name: String, ac: Arrow) extends TypeDefinition
case class DataConstructor(name: String, args: List[Type]) extends Positional {
  override def toString = name + " " + args.mkString(" ")
}
