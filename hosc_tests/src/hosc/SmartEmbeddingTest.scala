package hosc

import org.junit.Test
import org.junit.Ignore

class SmartEmbeddingTest {
  @Test def example01_rev(): Unit = {    
    SuperCompilerApp.main(Array("-si", "se/in/01a_rev.hl0", "-t", "se/out/01a_rev.svg", "-p", "se/out/01a_rev.hl1"));
    SuperCompilerApp.main(Array("-si", "se/in/01b_rev.hl0", "-t", "se/out/01b_rev.svg", "-p", "se/out/01b_rev.hl1"));
    SuperCompilerApp.main(Array("-si", "se/in/01c_rev.hl0", "-t", "se/out/01c_rev.svg", "-p", "se/out/01c_rev.hl1"));
  }
  
  @Test def example03_comp_map_join(): Unit = {    
    SuperCompilerApp.main(Array("-si", "se/in/03a_comp_map_join.hl0", "-t", "se/out/03a_comp_map_join.svg", "-p", "se/out/03a_comp_map_join.hl1"));
    SuperCompilerApp.main(Array("-si", "se/in/03b_comp_map_join.hl0", "-t", "se/out/03b_comp_map_join.svg", "-p", "se/out/03b_comp_map_join.hl1"));
  }
}
