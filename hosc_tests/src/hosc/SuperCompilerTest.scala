package hosc

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import HLanguage._
import Util._
import sc0.SuperCompiler0
import sc0.SuperCompiler0App

class SuperCompilerTest {
  
  @Test{val timeout = 9000} def t01_loop(): Unit = {
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/01_loop.hl",
        "-t", "output/01_loop.svg",
        "-p", "output/01_loop.hl"));
    }
  @Ignore
  @Test def loop(): Unit = {
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/loop.hl",
        "-t", "output/loop.svg",
        "-p", "output/loop.hl"));
    }
  
  @Test def eq(): Unit = {
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/eq.hl",
        "-t", "output/eq.svg",
        "-p", "output/eq.hl"));
    }
  @Test def eqnum_plus(): Unit = {
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/eqnum_plus.hl",
        "-t", "output/eqnum_plus.svg",
        "-p", "output/eqnum_plus.hl"));
    }
  @Test def t02_ham(): Unit = {
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/02_ham.hl",
        "-t", "output/02_ham.svg",
        "-p", "output/02_ham.hl"));
    }
  
  @Test def t03_revs1(): Unit = {
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/revs1.hl",
        "-t", "output/revs1.svg",
        "-p", "output/revs1.hl"));
    }
  @Test def rev_2(): Unit = {
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/rev_2.hl",
        "-t", "output/rev_2.svg",
        "-p", "output/rev_2.hl"));
    }
  
  @Test def rev_3(): Unit = {
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/rev_3.hl",
        "-t", "output/rev_3.svg",
        "-p", "output/rev_3.hl"));
    }
  
  @Test def simple(): Unit = {
    testSC("hl/supercompiler/rev1.hl")
    testSC("hl/supercompiler/rev2.hl")
    testSC("hl/supercompiler/rev3.hl")
  }  
  
  def testSC(fileName: String): Unit = {
    val p = programFromFile(fileName)
    val sc = new SuperCompiler0(p)
    val tree = sc.buildProcessTree(p.goal)
    println(tree)
  }
  
  @Test def processSamples(): Unit =
  {
    
    
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/rev1.hl",
        "-t", "output/app1_1.svg",
        "-p", "output/app1.hl"));
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/rev2.hl",
        "-t", "output/app2_1.svg",
        "-p", "output/app2.hl"));    
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/rev3.hl",
        "-t", "output/app3_1.svg",
        "-p", "output/app3.hl"));
    
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/app.hl",
        "-t", "output/z02.svg",
        "-p", "output/z02.hl"));
        
   SuperCompiler0App.main(Array("-si", "hl/supercompiler/f.hl",
            "-t", "output/z03.svg",
            "-p", "output/z03.hl"));   
   
   SuperCompiler0App.main(Array("-si", "hl/supercompiler/mapNotNot1.hl", 
       "-t", "output/mapNotNot.svg",
       "-p", "output/mapNotNot.hl"));
   
   SuperCompiler0App.main(Array("-si", "hl/supercompiler/mapNotNot2.hl",
       "-t", "output/notNot.svg",
       "-p", "output/notNot.hl"));
   
   SuperCompiler0App.main(Array("-si", "hl/supercompiler/mapNotNot3.hl",
       "-t", "output/mapNot.svg",
       "-p", "output/mapNot.hl"));
   
   SuperCompiler0App.main(Array("-si", "hl/supercompiler/mapNotNot4.hl",
       "-t", "output/mapx.svg",
       "-p", "output/mapx.hl"));
   
   SuperCompiler0App.main(Array("-si", "hl/supercompiler/mapNotNot5.hl",
       "-t", "output/id.svg",
       "-p", "output/id.hl"));
   
   SuperCompiler0App.main(Array("-si", "hl/supercompiler/eqnumxx.hl",
       "-t", "output/eqnumxx.svg",
       "-p", "output/eqnumxx.hl"));
   
   SuperCompiler0App.main(Array("-si", "hl/supercompiler/app1.hl",
       "-t", "output/app1.svg",
       "-p", "output/app.hl"));
  }
  
  @Test def takenm(): Unit = {
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/takenm.hl",
        "-t", "output/takenm.svg",
        "-p", "output/takenm.hl"));
  }
  
  @Test def eqnum(): Unit = {
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/eqnum.hl",
        "-t", "output/eqnum.svg",
        "-p", "output/eqnum.hl"));
  }
  
  
  @Test def synapse(): Unit = {
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/synapse.hl",
        "-t", "output/synapse.svg",
        "-p", "output/synapse.hl"));
    /*
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/synapse2.hl",
        "-t", "output/synapse1.svg",
        "-p", "output/synapse1.hl"));*/
  }
  
  @Test def regexp(): Unit = {
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/regexp.hl",
        "-t", "output/regexp.svg",
        "-p", "output/regexp.hl"));
  }
  
  @Test def even(): Unit = {
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/even.hl",
        "-t", "output/even.svg",
        "-p", "output/even.hl"));
    
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/even2.hl",
        "-t", "output/even2.svg",
        "-p", "output/even2.hl"));
  }
  
  @Test def evenSS(): Unit = {
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/evenSS.hl",
        "-t", "output/evenSS.svg",
        "-p", "output/evenSS.hl"));
    
  }
  
  @Test def caze(): Unit = {
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/case.hl",
        "-t", "output/case.svg",
        "-p", "output/case.hl"));
  }
  
  @Test def `var`(): Unit = {
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/var.hl",
        "-t", "output/var.svg",
        "-p", "output/var.hl"));
  }
  
  @Test def rev(): Unit = {
  SuperCompiler0App.main(Array("-si", "hl/supercompiler/rev.hl",
      "-t", "output/rev.svg",
      "-p", "output/rev.hl"));
  }
  
  // TODO: this input is supercompiled in loop
  // allow def to be not lambda only but any term
	@Ignore
  @Test def rev1(): Unit = {
  SuperCompiler0App.main(Array("-si", "hl/supercompiler/sc_rev1.hl",
      "-t", "output/sc_rev1.svg",
      "-p", "output/sc_rev1.hl"));
  }
  
  @Test def rev_1(): Unit = {
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/rev_1.hl",
        "-t", "output/rev_11.svg",
        "-p", "output/rev_11.hl"));
    }
  
  @Test def even1(): Unit = {
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/even1.hl",
        "-t", "output/even1.svg",
        "-p", "output/even1.hl"));
    }
  
  @Test def exp(): Unit = {
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/exp.hl",
        "-t", "output/exp.svg",
        "-p", "output/exp.hl"));
    }
  
  @Test def letrec(): Unit = {
    SuperCompiler0App.main(Array("-si", "hl/supercompiler/letrec.hs",
        "-t", "output/letrec.svg",
        "-p", "output/letrec.hs"));
    }
}
