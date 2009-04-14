package hosc

import org.junit.Test
import org.junit.Ignore

class PresentationExamples {
  @Test def example01_map_comp(): Unit = {    
    SuperCompilerApp.main(Array("-si", "pres/in/01a_map_comp.hl0", "-t", "pres/out/01a_map_comp.svg", "-p", "pres/out/01a_map_comp.hl1"));
    SuperCompilerApp.main(Array("-si", "pres/in/01b_map_comp.hl0", "-t", "pres/out/01b_map_comp.svg", "-p", "pres/out/01b_map_comp.hl1"));
  }
  
  @Test def example02_comp_map_unit(): Unit = {    
    SuperCompilerApp.main(Array("-si", "pres/in/02a_comp_map_unit.hl0", "-t", "pres/out/02a_comp_map_unit.svg", "-p", "pres/out/02a_comp_map_unit.hl1"));
    SuperCompilerApp.main(Array("-si", "pres/in/02b_comp_map_unit.hl0", "-t", "pres/out/02b_comp_map_unit.svg", "-p", "pres/out/02b_comp_map_unit.hl1"));
  }
  
  @Test def example03_comp_map_join(): Unit = {    
    SuperCompilerApp.main(Array("-si", "pres/in/03a_comp_map_join.hl0", "-t", "pres/out/03a_comp_map_join.svg", "-p", "pres/out/03a_comp_map_join.hl1"));
    //SuperCompilerApp.main(Array("-si", "pres/in/03b_comp_map_join.hl0", "-t", "pres/out/03b_comp_map_join.svg", "-p", "pres/out/03b_comp_map_join.hl1"));
  }
  
  @Test def example04_2append(): Unit = {    
    SuperCompilerApp.main(Array("-si", "pres/in/04a_2append.hl0", "-t", "pres/out/04a_2append.svg", "-p", "pres/out/04a_2append.hl1"));
    SuperCompilerApp.main(Array("-si", "pres/in/04b_2append.hl0", "-t", "pres/out/04b_2append.svg", "-p", "pres/out/04b_2append.hl1"));
  }
  
}
