package hosc

import org.junit.Test
import org.junit.Ignore
import sc0.SuperCompiler0App

class SmartEmbeddingTest {
  @Test def example03_comp_map_join(): Unit = {    
    SuperCompiler0App.main(Array("-si", "se/in/03a_comp_map_join.hl0", "-t", "se/out/03a_comp_map_join.svg", "-p", "se/out/03a_comp_map_join.hl1"));
    SuperCompiler0App.main(Array("-si", "se/in/03b_comp_map_join.hl0", "-t", "se/out/03b_comp_map_join.svg", "-p", "se/out/03b_comp_map_join.hl1"));
  }
}
