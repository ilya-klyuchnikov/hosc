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
     def  \\(s: Map[Variable, Variable]): Variable =
       s.get(this) match {case Some(t) => t; case None => this}
     def / (s: Map[Variable, Expression]): Expression =
       s.get(this) match {case Some(t) => t; case None => this}
     var global = false // global var is call
     override def toString: String =
       name
     def toDoc: Document =
       text(name)
   } 
   case class Constructor(name: String, args: List[Expression]) extends Expression {
     type termType = Constructor
     def \\(s: Map[Variable, Variable]): Constructor =
       Constructor(name, args map {_\\s})
     def / (s: Map[Variable, Expression]): Expression =
       Constructor(name, args map {_/s})
     override def toString: String =
       "(" + name + (args match {case Nil => ""; case _ => args.mkString(" ", " ","")}) + ")"
     def toDoc: Document =
       args match {
        case Nil => text(name)
        case _ => group(("(" + name) :: nest(2, ED :: args.foldRight(ED){(x, y) => ED :/: x.toDoc :: y}) :: ")" :: ED)
      }
   }
   case class LambdaAbstraction(v: Variable, t: Expression) extends Expression {
     type termType = LambdaAbstraction
     def \\(s: Map[Variable, Variable]): LambdaAbstraction =
       LambdaAbstraction(v\\s, t\\s)
     def / (s: Map[Variable, Expression]): Expression =
       LambdaAbstraction(v, t/s)
     override def toString: String =
       "(\\" + v.name + " -> " + t + ")"
     def toDoc: Document =
       "(\\" :: v.toDoc :: "->" :: nest(2, ED :/: t.toDoc) :: ")" :: ED
   }
   case class Application(head: Expression, arg: Expression) extends Expression {
     type termType = Application
     def \\(s: Map[Variable, Variable]): Application =
       Application(head\\s, arg\\s)
     def / (s: Map[Variable, Expression]) = Application(head/s, arg/s)
     pos = head.pos
     override def toString: String =
       "(" + head + " " + arg + ")"
     def toDoc: Document =
       group("(" :: nest(2, head.toDoc :/: arg.toDoc ) :: ")" :: ED)
   } 
   case class CaseExpression(selector: Expression, branches: List[Branch]) extends Expression {
     type termType = CaseExpression
     def \\(s: Map[Variable, Variable]): CaseExpression =
       CaseExpression(selector\\s, branches map {_\\s})
     def / (s: Map[Variable, Expression]): Expression =
       CaseExpression(selector/s, branches map { b => Branch(b.pattern, b.term / s)})
     override def toString: String =
       "case (" + selector + ") of " + branches.mkString("{", " ", "}")
     def toDoc: Document =
       group( group("case " :/: selector.toDoc :/: " of {" :: ED) :: nest(2, branches.foldRight(ED){(b, y) => ED :/: b.toDoc :: y}) :/: "}" :: ED)
   }
   case class LetExpression(bs: List[(Variable, Expression)], expr: Expression) extends Expression {
     type termType = LetExpression
     def \\(s: Map[Variable, Variable]): LetExpression =
       LetExpression(bs map { b => (b._1\\s, (b._2\\s).asInstanceOf[Expression])}, expr\\s)
     def / (s: Map[Variable, Expression]): Expression =
       LetExpression(bs map { b => (b._1, b._2/s)}, expr/s)
     override def toString: String =
       "let " + (bs map { p => p._1 + "=" + p._2}).mkString(", ") + "\n in " + expr
     def toDoc: Document =
       group("let" ::
           nest(2, bs.foldLeft(ED){(y, b) => y :/: group (b._1.toDoc :: " = " :: b._2.toDoc)})
           :/: "in " :: nest(2, ED :/: expr.toDoc))
   }
   case class LetRecExpression(binding: (Variable, Expression), expr: Expression) extends Expression {
     type termType = LetRecExpression
     def \\(s: Map[Variable, Variable]): LetRecExpression =
       LetRecExpression((binding._1\\s, binding._2\\s), expr\\s)
     def / (s: Map[Variable, Expression]): Expression =
       LetRecExpression((binding._1, binding._2/s), expr/s)
     override def toString: String =
       "(letrec " + (binding._1 + "=" + binding._2) + "\n in " + expr + ")"
     def toDoc: Document =
       group("(letrec" ::
         nest(2, group (ED :/: binding._1.toDoc :: "=" :: binding._2.toDoc))
         :/: "in" :: nest(2, ED :/: expr.toDoc) :: ")" :: ED)
   }
   
   case class Branch(pattern: Pattern, term: Expression) extends Positional {
     def \\(s: Map[Variable, Variable]): Branch = Branch(pattern\\s, term\\s)
     override def toString: String =
       pattern + " -> " + term + ";"
     def toDoc: Document =
       group(pattern.toDoc :: " ->" :: nest(2 , ED :/: term.toDoc :: ";" :: ED))
   }
   case class Pattern(name: String, args: List[Variable]) extends Positional {
     def \\(s: Map[Variable, Variable]) = Pattern(name, args map {_\\s})
     override def toString: String =
       name + " " + args.mkString(" ")
     def toDoc: Document =
       text(toString)
   }
   
   case class Function(name: String, body: Expression) extends Positional {
     def toDoc: Document =
       text(name) :: text(" = ") :: body.toDoc :: ";" :: ED
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
     def toDoc: Document = (ED /: ts) { (doc, td) => doc :/: nest(0, text(td.toString))} :/:
                 ED :/: nest(0, goal.toDoc) :/: 
                 (if (fs.isEmpty) ED else text("where") :/: (ED /: fs) {(doc, f) => doc :/: nest(0, f.toDoc)})
     
     def toDocString: String = {
       val doc1 = toDoc    
       val writer1 = new java.io.StringWriter()
       doc1.format(120, writer1)
       writer1.toString
     }
   }
}

sealed abstract class Type extends Positional
case class TypeVariable(name: String) extends Type {
  override def toString: String =
    name
}
case class TypeConstructor(name: String, typeParameters: List[Type]) extends Type {
  override def toString: String = typeParameters match {
    case Nil => name
    case _   => "(" + name + " " + typeParameters.mkString(" ") + ")";
  }
}
case class Arrow(t1: Type, t2: Type) extends Type {
  pos = t1.pos
  override def toString: String =
    "(" + t1 + "->" + t2 + ")"
}
 
abstract sealed class TypeDefinition extends Positional {
  def name: String
}
case class TypeConstructorDefinition(name: String, args: List[TypeVariable], cons: List[DataConstructor])
  extends TypeDefinition {
  override def toString: String =
    "data " + name + " " + args.mkString(" ") + " = " + cons.mkString(" | ") + ";"
}
case class DataConstructor(name: String, args: List[Type]) extends Positional {
  override def toString: String =
    name + " " + args.mkString(" ")
}
