package hosc;

/** 
 * used by residual program generator
 */
object HLanguage1 {
   sealed abstract class Expression1
   
   case class Variable1(name: String) extends Expression1 {
     var global = false // global var is call
     override def toString = name 
   } 
   case class Constructor1(name: String, args: List[Expression1]) extends Expression1 {
     override def toString = "(" + name + " " + args.mkString(" ") + ")"
   }
   case class LambdaAbstraction1(v: Variable1, t: Expression1) extends Expression1 {
     override def toString = "%" + v.name + " {" + t + "}" 
   }
   case class Application1(head: Expression1, arg: Expression1) extends Expression1 {
     override def toString = "(" + head + " " + arg + ")"
   } 
   case class CaseExpression1(selector: Expression1, branches: List[Branch1]) extends Expression1 {
     override def toString = "case (" + selector + ") of " + branches.mkString("{", " ", "}")  
   }
   case class LetExpression1(bs: List[Pair[Variable1, Expression1]], expr: Expression1) extends Expression1 {
     override def toString = "let " + (bs map {p => p._1 + "=" + p._2}).mkString(", ") + " in " + expr  
   }
   case class LetRecExpression1(bs: List[Pair[Variable1, Expression1]], expr: Expression1) extends Expression1 {
     override def toString = "letrec " + (bs map {p => p._1 + "=" + p._2}).mkString(", ") + " in " + expr  
   }
   
   case class Branch1(pattern: Pattern1, term: Expression1) {
     override def toString = pattern + " : " + term + ";"
   }
   case class Pattern1(name: String, args: List[Variable1]) {
     override def toString = name + " " + args.mkString(" ")  
   }
   
}
