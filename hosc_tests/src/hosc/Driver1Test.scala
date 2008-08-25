package hosc;

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import sc1.TermAlgebra1._
import sc1.MSG1._
import sc1.HE1.{he, heByCoupling}
import HLanguage1.{Application1 => A, Variable1 => V, CaseExpression1 => CE, Branch1 => B, Pattern1 => P,
  Constructor1 => C, LambdaAbstraction1 => LA, LetRecExpression1 => LR, _}
import hosc.LangUtils.{canonize1 => can, format => form}
import hosc.sc1.Driver1
import hosc.sc1.VarGen1

class Driver1Test {
  @Test def simple1(): Unit = {
    val t1String=
    """letrec
          f = %w1 {
            case  w1  of {
              Nil : Nil;
              Cons z1 u1 : letrec g = %p1 { 
                                  case  p1  of { 
                                          Cons r y1 : Cons r (g y1); 
                                          Nil : Cons z1 Nil; 
                                  } 
                           } 
                           in  g (f u1);
            }
          }
        in f x"""
    
    val t1 = can(TestUtils1.termFromString(t1String))
    println(form(t1))
    println
    val letrec = t1.asInstanceOf[LR]
    val unfolded = new Driver1(new VarGen1()).unfold(letrec)
    println(form(unfolded))    
  }
  
  @Test def simple2(): Unit = {
    val t1String=
    """letrec f8=%v100 {
          case (v100) of {
            S v61 : (f8 v61); 
            Z : (True );
          }
       } 
       in (f8 yy)"""
    
    val t1 = can(TestUtils1.termFromString(t1String))
    println(form(t1))
    println
    val letrec = t1.asInstanceOf[LR]
    val unfolded = new Driver1(new VarGen1()).unfold(letrec)
    println(form(unfolded))    
  }
  
  @Test def simple3(): Unit = {
    val t1String=
    """( letrec g = %t1 { %x2 { case  t1  of { Leaf v1 : x2; Node v y1 : ((g v) ((g y1) x2)); } } }
    in 
    ((g t)
      ( letrec f = %r1 { %s1 { case  r1  of { Leaf x1 : (Cons x1 s1); Node w z : ((f z) ((f w) s1)); } } }
      in 
        ((f t) l))))"""
    
    val t1 = can(TestUtils1.termFromString(t1String))
    println(form(t1))
    println
    val letrec = t1.asInstanceOf[LR]
    val unfolded = new Driver1(new VarGen1()).unfold(letrec)
    println(form(unfolded))    
  }
}
