package hosc.lemmas

import hosc.HLanguage._
import LemmaGenerator._

class LemmaGenerator(val funs: List[Variable], val cons: List[Ctr]) {
 
  def genAllExprs(maxSize: Int, vars: List[Variable]): List[Expression] =
    List.flatten(List.range(1, maxSize + 1) map {genExps(_, vars)})
  
  // vars = free vars
  private def genExps(size: Int, vars: List[Variable]): List[Expression] =  size match {
    case 1 => vars ++ funs ++ (cons filter {_.arity == 0} map {c => Constructor(c.name, Nil)} )
    case m if m <= 0 => Nil
    case n => {
      val appLs = parts(size, 2) map {
        case x :: y :: Nil => for (head <- genExps(x, vars); arg <- genExps(y, vars)) yield Application(head, arg)
      }
      val ctrLs = for (ctr <- cons) yield {
        val ps = parts(size - 1, ctr.arity)
        List.flatten(ps map {combExps(_, vars)}) map {Constructor(ctr.name, _)}
      }
      return List.flatten(appLs) ++ List.flatten(ctrLs)
    }
  }
  
  def combExps(is: List[Int], vars: List[Variable]):List[List[Expression]] = {
    is match {
      case Nil => List(Nil)
      case x :: xs => { 
        val xxx = genExps(x, vars)
        val xxx2 = combExps(xs, vars)
        for (e1 <- xxx; es <- xxx2) yield e1 :: es
      }
    }
  }
  
}

object LemmaGenerator {
  
  case class Ctr(name: String, arity: Int)
  
  def apply(program: Program) = {
    val funs: List[Variable] = program.fs map {_.name} map {n => val v = Variable(n); v.global = true; v}
    val cons: List[Ctr] = List.flatten(program.ts map {_.cons}) map {dc => Ctr(dc.name, dc.args.length)}
    new LemmaGenerator(funs, cons)
  }
  
  def parts(size:Int, arity:Int): List[List[Int]] = 
    if (arity > size || arity <0) Nil 
    else if (size <= 0) List(Nil)
    else List.flatten(List.range(1, size - arity + 2) map {x => parts(size - x, arity - 1) map {x::_}})  
}