package hosc

import org.junit.Test
import org.junit.Ignore
import sc0.SuperCompiler0App

class PresentationExamples {
  @Test def examples(): Unit = {    
    SuperCompiler0App.main(Array("-si", "pres/in/01a_map_comp.hl0", "-t", "pres/out/01a_map_comp.svg", "-p", "pres/out/01a_map_comp.hl1"));
    SuperCompiler0App.main(Array("-si", "pres/in/01b_map_comp.hl0", "-t", "pres/out/01b_map_comp.svg", "-p", "pres/out/01b_map_comp.hl1"));
    SuperCompiler0App.main(Array("-si", "pres/in/02a_comp_map_unit.hl0", "-t", "pres/out/02a_comp_map_unit.svg", "-p", "pres/out/02a_comp_map_unit.hl1"));
    SuperCompiler0App.main(Array("-si", "pres/in/02b_comp_map_unit.hl0", "-t", "pres/out/02b_comp_map_unit.svg", "-p", "pres/out/02b_comp_map_unit.hl1"));
    SuperCompiler0App.main(Array("-si", "pres/in/03a_comp_map_join.hl0", "-t", "pres/out/03a_comp_map_join.svg", "-p", "pres/out/03a_comp_map_join.hl1"));
    SuperCompiler0App.main(Array("-si", "pres/in/03b_comp_map_join.hl0", "-t", "pres/out/03b_comp_map_join.svg", "-p", "pres/out/03b_comp_map_join.hl1"));
  }
  
}
