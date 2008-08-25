package hosc

import org.junit.Test
import org.junit.Ignore
import sc2.SuperCompiler2App

class SuperCompiler2Test {
  @Test def revert1(): Unit ={    
    SuperCompiler2App.main(Array("-i", "hl1/sc2/revert1.hl1",
        "-t", "output/hl1/sc2/revert1.svg",
        "-p", "output/hl1/sc2/revert1.hl1"));
  }
  
  @Test def revert2(): Unit ={    
    SuperCompiler2App.main(Array("-i", "hl1/sc2/revert2.hl1",
        "-t", "output/hl1/sc2/revert2.svg",
        "-p", "output/hl1/sc2/revert2.hl1"));
  }
  
  @Test def eqnumplus2: Unit ={    
    SuperCompiler2App.main(Array("-i", "hl1/sc2/eqnumplus2.hl1",
        "-t", "output/hl1/sc2/eqnumplus2.svg",
        "-p", "output/hl1/sc2/eqnumplus2.hl1"));
  }
  
  @Test def eqnumplus_snippet: Unit ={    
    SuperCompiler2App.main(Array("-i", "hl1/sc2/eqnumplus_snippet.hl1",
        "-t", "output/hl1/sc2/eqnumplus_snippet.svg",
        "-p", "output/hl1/sc2/eqnumplus_snippet.hl1"));
  }
  
  @Test def leq_add_acc: Unit ={    
    SuperCompiler2App.main(Array("-i", "hl1/sc2/leq_add_acc.hl1",
        "-t", "output/hl1/sc2/leq_add_acc.svg",
        "-p", "output/hl1/sc2/leq_add_acc.hl1"));
  }
  
  @Test def app_rev: Unit ={    
    SuperCompiler2App.main(Array("-i", "hl1/sc2/app_rev.hl1",
        "-t", "output/hl1/sc2/app_rev.svg",
        "-p", "output/hl1/sc2/app_rev.hl1"));
  }
  
  @Test def snippet1(): Unit ={    
    SuperCompiler2App.main(Array("-i", "hl1/sc2/snippet1.hl1",
        "-t", "output/hl1/sc2/snippet1.svg",
        "-p", "output/hl1/sc2/snippet1.hl1"));
  }
  
  @Test def snippet2(): Unit ={    
    SuperCompiler2App.main(Array("-i", "hl1/sc2/snippet2.hl1",
        "-t", "output/hl1/sc2/snippet2.svg",
        "-p", "output/hl1/sc2/snippet2.hl1"));
  }
  
}
