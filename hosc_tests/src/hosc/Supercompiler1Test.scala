package hosc

import org.junit.Test
import org.junit.Ignore
import sc1.SuperCompiler1App
import sc1.SuperCompiler1SApp

class Supercompiler1Test {
  
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
