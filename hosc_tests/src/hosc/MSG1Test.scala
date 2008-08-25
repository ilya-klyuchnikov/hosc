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

class MSG1Test {

  @Test def msg01(): Unit = {
    val appCall = V("app")
    appCall.call = true
    val msg_ = msg(appCall, appCall)
    println("XXXXXXXXXXXXXXX")
    println(appCall)
    println(appCall)
    println(msg_)
    assertEquals(msg_.term, appCall)
  }
  
  @Test def msg02(): Unit = {
    val appCall = V("app")
    appCall.call = true
    val revCall = V("rev")
    revCall.call = true
    val msg_ = msg(appCall, revCall)
    println("XXXXXXXXXXXXXXX")
    println(appCall)
    println(revCall)
    println(msg_)
  }
  
  @Test def msg03(): Unit = {
    val appCall1 = V("app")
    appCall1.call = true
    val appCall2 = V("app")
    appCall2.call = true
    
    val app1 = A(appCall1, C("S", V("x")::Nil))
    val app2 = A(appCall2, C("S", C("S", V("y")::Nil)::Nil))
    val msg_ = msg(app1, app2)
    println("XXXXXXXXXXXXXXX")
    println(app1)
    println(app2)
    println(msg_)
    assertTrue(instanceOf(app1, app2))
    
    app1.label = Repeat()
    val msg1_ = msg(app1, app2)
    println("XXXXXXXXXXXXXXX")
    println(app1)
    println(app2)
    println(msg1_)
  }
  
  @Test def msg04(): Unit = {
    val f1 = V("f1")
    val f1Call = V("f1"); f1Call.call = true
    val lr1 = LR((f1Call, LA(V("y"), A(f1Call, V("y")))), A(f1Call, V("x2")))
    
    val f2 = V("f2")
    val f2Call = V("f2"); f2Call.call = true
    val lr2 = LR((f2Call, LA(V("y"), A(f2Call, V("y")))), A(f2Call, V("x1")))
    
    val msg_ = msg(lr1, lr2)
    val smsg_ = strongMsg(lr1, lr2)
    println(lr1)
    println(lr2)
    println(msg_)
    println(smsg_)
    assertTrue(instanceOf(lr1, lr2))
  }
  
  @Test def msg05(): Unit = {
    val f1Call = V("f1"); f1Call.call = true
    val la1 = LA(V("y"), A(f1Call, V("y")))
    val f2 = V("f2")
    val f2Call = V("f1"); f2Call.call = true
    val la2 = LA(V("z"), A(f2Call, V("z")))
    
    val msg_ = msg(la1, la2)
    val smsg_ = strongMsg(la1, la2)
    println("XXXXXXXXXXXXXXX")
    println(la1)
    println(la2)
    println(msg_)
    println(smsg_)
    assertTrue(instanceOf(la1, la2))
  }
  
  @Test def msg100(): Unit = {
    val t1 = TestUtils1.termFromFile("hl/sc1/z1.hl")
    val t2 = TestUtils1.termFromFile("hl/sc1/z2.hl")
    println(t1)
    println(t2)
    val msg1 = msg(t1, t2)
    println(msg1.term)
    println(msg1.sub1)
    println(msg1.sub2)
    println(he(t1, t2))
    //assertTrue(equivalent(msg1.term, t1))
  }
  
  @Test def msg101(): Unit = {
    val t1 = TestUtils1.termFromFile("hl/sc1/z3a.hl")
    val t2 = TestUtils1.termFromFile("hl/sc1/z3b.hl")
    println(t1)
    println(t2)
    val msg1 = msg(t1, t2)
    println(msg1.term)
    println(msg1.sub1)
    println(msg1.sub2)
    println(he(t1, t2))
    println(instanceOf(t1, t2))
  }
  
  @Test def msg201(): Unit = {
    val t1 = can(TestUtils1.termFromFile("hl/sc1/msg1a.hl"))
    val t2 = can(TestUtils1.termFromFile("hl/sc1/msg1b.hl"))
    println(form(t1))
    println(form(t2))
    val msg1 = msg(t1, t2)
    println(form(msg1.term))
    println(msg1.sub1)
    println(msg1.sub2)
    println(he(t1, t2))
    println(instanceOf(t1, t2))
    assertFalse(instanceOf(t1, t2))
    assertTrue(he(t1, t2))
  }
  
  @Test def msg202(): Unit = {
    val t1 = can(TestUtils1.termFromFile("hl/sc1/msg2a.hl"))
    val t2 = can(TestUtils1.termFromFile("hl/sc1/msg2b.hl"))
    println(form(t1))
    println(form(t2))
    val msg1 = msg(t1, t2)
    println(form(msg1.term))
    println(msg1.sub1)
    println(msg1.sub2)
    println(he(t1, t2))
    println(he(t2, t1))
    println(instanceOf(t1, t2))
    //println(instanceOf(t1, t2))
    assertFalse(instanceOf(t1, t2))
  }
  
  @Test def msg203(): Unit = {
    val (t1String, t2String)=
    ("""Loop: case (n) of {
      S oo7 : Repeat: case (((double oo7) (S (S z)))) of {S oo10 : case (oo10) of {S oo11 : (even oo11); Z : (False );}; Z : (True );}; 
      Z : letrec foo1=%oo208 {case (oo208) of {Z : (True ); S oo67 : case (oo67) of {Z : (False ); S oo81 : (foo1 oo81);};}} in (foo1 z);}""",
     """Loop: case (oo230) of {
        S oo245 : Repeat: case (((double oo245) (S (S (S (S z)))))) of {S oo254 : case (oo254) of {S oo255 : (even oo255); Z : (False );}; Z : (True );}; 
        Z : letrec foo3=%oo475 {case (oo475) of {Z : (True ); S oo348 : case (oo348) of {Z : (False ); S oo366 : (foo3 oo366);};}} in (foo3 z);}""");
    
    val t1 = can(TestUtils1.termFromString(t1String))
    val t2 = can(TestUtils1.termFromString(t2String))
    println(t1)
    println(t2)
    val msg1 = msg(t1, t2)
    val msgTerm = can(msg1.term)
    println(msgTerm)
    println(msg1.sub1)
    println(msg1.sub2)
    println(he(t1, t2))
    assertFalse(instanceOf(t1, t2))
  }
  
  @Test def msg204(): Unit = {
    val (t1String, t2String)=
    ("""letrec foo60=%oo2277 {%oo2278 {%oo2279 {case (oo2277) of {S oo1371 : (((foo60 oo1371) (S (S oo2278))) oo2278); Z : letrec foo4=%oo2169 {case (oo2169) of {S oo1696 : case (oo1696) of {S oo1711 : (foo4 oo1711); Z : (False );}; Z : (True );}} in (foo4 oo2279);}}}} in (((foo60 n) z) z)""",
     """letrec foo10=%oo3717 {%oo3718 {%oo3719 {case (oo3718) of {S oo2844 : (((foo10 (S (S oo3717))) oo2844) oo3717); Z : letrec foo8=%oo3563 {case (oo3563) of {S oo3165 : case (oo3165) of {S oo3184 : (foo8 oo3184); Z : (False );}; Z : (True );}} in (foo8 oo3719);}}}} in (((foo10 z) n) z)""");
    
    val t1 = can(TestUtils1.termFromString(t1String))
    val t2 = can(TestUtils1.termFromString(t2String))
    println(form(t1))
    println(form(t2))
    val msg1 = msg(t1, t2)
    val msgTerm = can(msg1.term)
    println(form(msgTerm))
    println(msg1.sub1)
    println(msg1.sub2)
    println(he(t1, t2))
    assertFalse(instanceOf(t1, t2))
  }
  
  @Test def msg205(): Unit = {
    val (t1String, t2String)=
    ("""letrec foo1=%oo174 {
          case (oo174) of {
            Cons oo77 oo78 : (Cons oo77 (foo1 oo78)); 
            Nil : (Cons oo2 (Nil ));
          }
       } 
       in (foo1 (f oo3))""",
     """letrec foo2=%oo681 {
         case (oo681) of {
            Cons oo472 oo473 : (Cons oo472 (foo2 oo473)); 
            Nil : (Cons oo205 (Cons oo2 (Nil )));
         }
        } 
        in (foo2 (f oo206))""");
    
    val t1 = can(TestUtils1.termFromString(t1String))
    val t2 = can(TestUtils1.termFromString(t2String))
    println(form(t1))
    println(form(t2))
    val msg1 = msg(t1, t2)
    val msgTerm = can(msg1.term)
    println(form(msgTerm))
    println(msg1.sub1)
    println(msg1.sub2)
    println(heByCoupling(t1, t2))
    assertFalse(instanceOf(t1, t2))
  }
  
  @Test def msg206(): Unit = {
    val (t1String, t2String)=
    ("""case x of {
      Z:Z;
      S x: f x;
    }""",
    """case x of {
      Z:Z;
      S x: g (f x);
    }""");
    
    val t1 = can(TestUtils1.termFromString(t1String))
    val t2 = can(TestUtils1.termFromString(t2String))
    println(form(t1))
    println(form(t2))
    val msg1 = msg(t1, t2)
    val msgTerm = can(msg1.term)
    println(form(msgTerm))
    println(msg1.sub1)
    println(msg1.sub2)
    println(heByCoupling(t1, t2))
    //assertFalse(instanceOf(t1, t2))
  }
  

  @Test def msgzz(): Unit = {
    val (t1String, t2String)=
    ("""letrec f=%p4 {%r4 {case (p4) of {S z3 : case (r4) of {S x2 : ((f z3) x2); Z : letrec f1=%z5 {%u5 {case (z5) of {S u2 : case (u5) of {S s1 : ((f1 u2) s1); Z : letrec g1=%w5 {%v5 {case (w5) of {S p1 : case (v5) of {S v1 : ((g1 p1) v1); Z : (False );}; Z : case (v5) of {S y2 : (False ); Z : (True );};}}} in ((g1 yy) u2);}; Z : (False );}}} in ((f1 xx) z3);}; Z : letrec g=%t4 {%s4 {case (t4) of {S z4 : case (s4) of {S s : ((g z4) s); Z : letrec h=%x5 {%y5 {case (x5) of {S u2 : case (y5) of {S z2 : ((h u2) z2); Z : case (u2) of {S r1 : (False ); Z : (True );};}; Z : (False );}}} in ((h xx) z4);}; Z : case (s4) of {S s3 : (False ); Z : case (xx) of {S r2 : (False ); Z : (True );};};}}} in ((g yy) r4);}}} in ((f xx) yy)""",
     """letrec foo10=%oo3717 {%oo3718 {%oo3719 {case (oo3718) of {S oo2844 : (((foo10 (S (S oo3717))) oo2844) oo3717); Z : letrec foo8=%oo3563 {case (oo3563) of {S oo3165 : case (oo3165) of {S oo3184 : (foo8 oo3184); Z : (False );}; Z : (True );}} in (foo8 oo3719);}}}} in (((foo10 z) n) z)""");
    
    val t1 = can(TestUtils1.termFromString(t1String))
    val t2 = can(TestUtils1.termFromString(t2String))
    println(form(t1))
    //println(form(t2))
    
  }
  
  @Test def msgyy(): Unit = {
    val (t1String, t2String)=
    ("case (voo3788) of {S voo3785 : letrec foo40=%voo3797 {%voo3803 {case (voo3797) of {S voo3808 : case (voo3803) of {S voo3816 : ((foo40 voo3808) voo3816); Z : letrec foo41=%voo3847 {%voo3856 {case (voo3847) of {S voo3864 : case (voo3856) of {S voo3879 : ((foo41 voo3864) voo3879); Z : letrec foo42=%voo3913 {%voo3919 {case (voo3913) of {S voo3929 : case (voo3919) of {S voo3935 : ((foo42 voo3929) voo3935); Z : (False );}; Z : case (voo3919) of {S voo3939 : (False ); Z : (True );};}}} in ((foo42 voo3864) voo3837);}; Z : case (voo3856) of {S voo3883 : (False ); Z : (False );};}}} in ((foo41 (S voo3855)) voo3808);}; Z : letrec foo43=%voo3838 {%voo3849 {case (voo3838) of {S voo3858 : case (voo3849) of {S voo3868 : ((foo43 voo3858) voo3868); Z : (False );}; Z : letrec foo44=%voo3891 {%voo3907 {case (voo3891) of {S voo3915 : case (voo3907) of {S voo3921 : ((foo44 voo3915) voo3921); Z : (False );}; Z : case (voo3907) of {S voo3925 : (False ); Z : (True );};}}} in ((foo44 (S voo3855)) voo3849);}}} in ((foo43 voo3803) voo3837);}}} in ((foo40 voo3795) voo3785); Z : letrec foo36=%voo3801 {%voo3806 {case (voo3801) of {S voo3812 : case (voo3806) of {S voo3823 : ((foo36 voo3812) voo3823); Z : letrec foo37=%voo3853 {%voo3862 {case (voo3853) of {S voo3875 : case (voo3862) of {S voo3893 : ((foo37 voo3875) voo3893); Z : (False );}; Z : case (voo3862) of {S voo3897 : (False ); Z : (True );};}}} in ((foo37 voo3812) voo3837);}; Z : case (voo3806) of {S voo3827 : (False ); Z : (False );};}}} in ((foo36 (S voo3855)) voo3795);}",
     "case (voo7010) of {S voo7007 : letrec foo81=%voo7019 {%voo7025 {case (voo7019) of {S voo7030 : case (voo7025) of {S voo7038 : ((foo81 voo7030) voo7038); Z : letrec foo82=%voo7069 {%voo7076 {case (voo7069) of {S voo7086 : case (voo7076) of {S voo7101 : ((foo82 voo7086) voo7101); Z : (False );}; Z : letrec foo83=%voo7128 {%voo7133 {case (voo7128) of {S voo7139 : case (voo7133) of {S voo7151 : ((foo83 voo7139) voo7151); Z : case (voo7139) of {S voo7159 : (False ); Z : (True );};}; Z : (False );}}} in ((foo83 voo7076) voo7058);}}} in ((foo82 voo7030) (S voo7075));}; Z : letrec foo84=%voo7059 {%voo7071 {case (voo7059) of {S voo7078 : case (voo7071) of {S voo7090 : ((foo84 voo7078) voo7090); Z : case (voo7078) of {S voo7110 : (False ); Z : (False );};}; Z : letrec foo85=%voo7116 {%voo7131 {case (voo7116) of {S voo7135 : case (voo7131) of {S voo7143 : ((foo85 voo7135) voo7143); Z : (False );}; Z : case (voo7131) of {S voo7147 : (False ); Z : (True );};}}} in ((foo85 voo7071) (S voo7075));}}} in ((foo84 voo7025) voo7058);}}} in ((foo81 voo7017) voo7007); Z : letrec foo77=%voo7023 {%voo7028 {case (voo7023) of {S voo7034 : case (voo7028) of {S voo7045 : ((foo77 voo7034) voo7045); Z : (False );}; Z : letrec foo78=%voo7065 {%voo7073 {case (voo7065) of {S voo7082 : case (voo7073) of {S voo7097 : ((foo78 voo7082) voo7097); Z : case (voo7082) of {S voo7120 : (False ); Z : (True );};}; Z : (False );}}} in ((foo78 voo7028) voo7058);}}} in ((foo77 voo7017) (S voo7075));}")
    
    val t1 = can(TestUtils1.termFromString(t1String))
    val t2 = can(TestUtils1.termFromString(t2String))
    println(form(t1))
    println(form(t2))
    val msg1 = msg(t1, t2)
    val msgTerm = can(msg1.term)
    println(form(msgTerm))
    println(msg1.sub1)
    println(msg1.sub2)
    println(heByCoupling(t1, t2))
    
  }
  
  @Test def WW(): Unit = {
    val (t1String, t2String)=
    (""" f a b""",
     """g a b""");
    
    val t1 = can(TestUtils1.termFromString(t1String))
    val t2 = can(TestUtils1.termFromString(t2String))
    println(form(t1))
    println(form(t2))
    val msg1 = msg(t1, t2)
    val msgTerm = can(msg1.term)
    println(form(msgTerm))
    println(msg1.sub1)
    println(msg1.sub2)
    println(heByCoupling(t1, t2))
    //assertFalse(instanceOf(t1, t2))
  }
  
  @Test def WWW(): Unit = {
    val (t1String, t2String)=
    (""" case (v562) of {S v559 : letrec vf81=%v569 {%v572 {case (v569) of {S v574 : ((vf81 v574) (S v572)); Z : (leqp v572);}}} in ((vf81 v559) (S v571)); Z : (leqp v571);}""",
     """ case (v632) of {S v629 : letrec vf91=%v639 {%v642 {case (v639) of {S v644 : ((vf91 v644) (S v642)); Z : (leqp v642);}}} in ((vf91 v629) (S v641)); Z : (leqp v641);}""");
    
    val t1 = can(TestUtils1.termFromString(t1String))
    val t2 = can(TestUtils1.termFromString(t2String))
    //println(form(t1))
    println(equivalent(t1, t2))
  }
  
}
