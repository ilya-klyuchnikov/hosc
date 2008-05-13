package hosc

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import HLanguage._
import Util._

class SuperCompilerTest {
  
  @Test def simple(): Unit = {
    testSC("hl/supercompiler/rev1.hl")
    testSC("hl/supercompiler/rev2.hl")
    testSC("hl/supercompiler/rev3.hl")
  }  
  
  def testSC(fileName: String): Unit = {
    val p = programFromFile(fileName)
    val sc = new SuperCompiler(p)
    val tree = sc.buildProcessTree(p.goal)
    println(tree)
  }
  
  @Test def processSamples(): Unit =
  {
    
    SuperCompilerApp.main(Array("-si", "hl/supercompiler/rev.hl",
        "-t", "output/rev_1.svg",
        "-p", "output/rev.hl"));
    SuperCompilerApp.main(Array("-si", "hl/supercompiler/rev1.hl",
        "-t", "output/app1_1.svg",
        "-p", "output/app1.hl"));
    SuperCompilerApp.main(Array("-si", "hl/supercompiler/rev2.hl",
        "-t", "output/app2_1.svg",
        "-p", "output/app2.hl"));    
    SuperCompilerApp.main(Array("-si", "hl/supercompiler/rev3.hl",
        "-t", "output/app3_1.svg",
        "-p", "output/app3.hl"));
    
    SuperCompilerApp.main(Array("-si", "hl/supercompiler/app.hl",
        "-t", "output/z02.svg",
        "-p", "output/z02.hl"));
        
   SuperCompilerApp.main(Array("-si", "hl/supercompiler/f.hl",
            "-t", "output/z03.svg",
            "-p", "output/z03.hl"));   
   
   SuperCompilerApp.main(Array("-si", "hl/supercompiler/mapNotNot1.hl", 
       "-t", "output/mapNotNot.svg",
       "-p", "output/mapNotNot.hl"));
   
   SuperCompilerApp.main(Array("-si", "hl/supercompiler/mapNotNot2.hl",
       "-t", "output/notNot.svg",
       "-p", "output/notNot.hl"));
   
   SuperCompilerApp.main(Array("-si", "hl/supercompiler/mapNotNot3.hl",
       "-t", "output/mapNot.svg",
       "-p", "output/mapNot.hl"));
   
   SuperCompilerApp.main(Array("-si", "hl/supercompiler/mapNotNot4.hl",
       "-t", "output/mapx.svg",
       "-p", "output/mapx.hl"));
   
   SuperCompilerApp.main(Array("-si", "hl/supercompiler/mapNotNot5.hl",
       "-t", "output/id.svg",
       "-p", "output/id.hl"));
   
   SuperCompilerApp.main(Array("-si", "hl/supercompiler/eqnumxx.hl",
       "-t", "output/eqnumxx.svg",
       "-p", "output/eqnumxx.hl"));
   
   SuperCompilerApp.main(Array("-si", "hl/supercompiler/app1.hl",
       "-t", "output/app1.svg",
       "-p", "output/app.hl"));
  }
  
  @Test def takenm(): Unit = {
    SuperCompilerApp.main(Array("-si", "hl/supercompiler/takenm.hl",
        "-t", "output/takenm.svg",
        "-p", "output/takenm.hl"));
  }
  
  @Test def eqnum(): Unit = {
    SuperCompilerApp.main(Array("-si", "hl/supercompiler/eqnum.hl",
        "-t", "output/eqnum.svg",
        "-p", "output/eqnum.hl"));
  }
  
  @Test def synapse(): Unit = {
    SuperCompilerApp.main(Array("-si", "hl/supercompiler/synapse.hl",
        "-t", "output/synapse.svg",
        "-p", "output/synapse.hl"));
  }
  
  @Test def regexp(): Unit = {
    SuperCompilerApp.main(Array("-si", "hl/supercompiler/regexp.hl",
        "-t", "output/regexp.svg",
        "-p", "output/regexp.hl"));
  }
  
}
