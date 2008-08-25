package hosc

import org.junit.Test
import org.junit.Ignore
import sc1.SuperCompiler1App
import sc1.SuperCompiler1SApp

class SuperCompiler1Test {
  @Test def commutativity0(): Unit ={    
    SuperCompiler1App.main(Array("-i", "hl/scp1/commutativity0.hl1",
        "-t", "output/scp1_commutativity0.svg",
        "-p", "output/scp1_commutativity0.hl1"));
    SuperCompiler1SApp.main(Array("-i", "hl/scp1/commutativity0.hl1",
        "-p", "output/scp1_commutativity0_stable.hl1"));
  }
  
  @Test def commutativity(): Unit ={    
    SuperCompiler1App.main(Array("-i", "hl/scp1/commutativity.hl1",
        "-t", "output/scp1_commutativity.svg",
        "-p", "output/scp1_commutativity.hl1"));
    //SuperCompiler1SApp.main(Array("-i", "hl/scp1/commutativity.hl1",
        //"-t", "output/scp1_commutativity.svg",
      //  "-p", "output/scp1_commutativity_stable.hl1"));
  }
  
  @Test def commutativity1(): Unit ={    
    SuperCompiler1App.main(Array("-i", "hl/scp1/commutativity1.hl1",
        "-t", "output/scp1_commutativity1.svg",
        "-p", "output/scp1_commutativity1.hl1"));
    SuperCompiler1SApp.main(Array("-i", "hl/scp1/commutativity1.hl1",
        //"-t", "output/scp1_commutativity1.svg",
        "-p", "output/scp1_commutativity1_stable.hl1"));
  }
  
  @Test def commutativity2(): Unit ={    
    SuperCompiler1App.main(Array("-i", "hl/scp1/commutativity2.hl1",
        "-t", "output/scp1_commutativity2.svg",
        "-p", "output/scp1_commutativity2.hl1"));
    //SuperCompiler1SApp.main(Array("-i", "hl/scp1/commutativity2.hl1",
        //"-t", "output/scp1_commutativity2.svg",
      //  "-p", "output/scp1_commutativity2_stable.hl1"));
  }
  
  @Test def stable(): Unit ={    
    SuperCompiler1SApp.main(Array("-i", "hl/scp1/commutativity.hl1",
        "-p", "output/scp1_commutativity_stable.hl1"));
  }
  
  @Test def g(): Unit ={    
    SuperCompiler1App.main(Array("-i", "hl1/sc1/g.hl1",
        "-t", "output/hl1/sc1/g.svg",
        "-p", "output/hl1/sc1/g.hl1"));
  }
  
  @Test def g2(): Unit ={    
    SuperCompiler1App.main(Array("-i", "hl1/sc1/g2.hl1",
        "-t", "output/hl1/sc1/g2.svg",
        "-p", "output/hl1/sc1/g2.hl1"));
  }
  
  @Test def eqnum_plus(): Unit ={    
    SuperCompiler1App.main(Array("-i", "hl1/sc1/eqnum_plus.hl1",
        "-t", "output/hl1/sc1/eqnum_plus.svg",
        "-p", "output/hl1/sc1/eqnum_plus.hl1"));
  }
  
  @Test def snippet1(): Unit ={    
    SuperCompiler1App.main(Array("-i", "hl1/sc1/snippet1.hl1",
        "-t", "output/hl1/sc1/snippet1.svg",
        "-p", "output/hl1/sc1/snippet1.hl1"));
  }
}
