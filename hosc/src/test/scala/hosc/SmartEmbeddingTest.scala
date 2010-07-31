package hosc

import org.junit.Test
import org.junit.Ignore

import hosc.exp.SuperCompilerApp1

class SmartEmbeddingTest {
  @Test def example01_rev(): Unit = {    
    SuperCompilerApp1.main(Array("-si", "se/in/01a_rev.hl0", "-t", "se/out/01a_rev.svg", "-p", "se/out/01a_rev.hl1"));
    SuperCompilerApp1.main(Array("-si", "se/in/01b_rev.hl0", "-t", "se/out/01b_rev.svg", "-p", "se/out/01b_rev.hl1"));
    SuperCompilerApp1.main(Array("-si", "se/in/01c_rev.hl0", "-t", "se/out/01c_rev.svg", "-p", "se/out/01c_rev.hl1"));
  }
  
  @Test def example03_comp_map_join(): Unit = {    
    SuperCompilerApp1.main(Array("-si", "se/in/03a_comp_map_join.hl0", "-t", "se/out/03a_comp_map_join.svg", "-p", "se/out/03a_comp_map_join.hl1"));
    SuperCompilerApp1.main(Array("-si", "se/in/03b_comp_map_join.hl0", "-t", "se/out/03b_comp_map_join.svg", "-p", "se/out/03b_comp_map_join.hl1"));
  }
  
  @Test def iter(): Unit = {    
    SuperCompilerApp1.main(Array("-si", "se/in/iter1.hs", "-t", "se/out/iter1.svg", "-p", "se/out/iter1.hs"));
    SuperCompilerApp1.main(Array("-si", "se/in/iter2.hs", "-t", "se/out/iter2.svg", "-p", "se/out/iter2.hs"));
  }
  
  @Test def filter(): Unit = {    
    SuperCompilerApp1.main(Array("-si", "se/in/filter1.hs", "-t", "se/out/filter1.svg", "-p", "se/out/filter1.hs"));
    SuperCompilerApp1.main(Array("-si", "se/in/filter2.hs", "-t", "se/out/filter2.svg", "-p", "se/out/filter2.hs"));
  }
  
  @Test def synapse(): Unit = {    
    SuperCompilerApp1.main(Array("-si", "se/in/synapse.hs", "-t", "se/out/synapse.svg", "-p", "se/out/synapse.hs"));
    //SuperCompilerApp.main(Array("-si", "se/in/synapse1.hs", "-t", "se/out/synapse1.svg", "-p", "se/out/synapse1.hs"));
    //SuperCompilerApp.main(Array("-si", "se/in/synapse2.hs", "-t", "se/out/synapse2.svg", "-p", "se/out/synapse2.hs"));
  }
}
