package hosc

import org.junit.Test
import org.junit.Ignore
import sc0.SuperCompiler0App

class TasksTest {
  @Test def task01_app(): Unit = {    
    SuperCompiler0App.main(Array("-si", "tasks/input/01a_app.hl0", "-t", "tasks/output/01a_app.svg", "-p", "tasks/output/01a_app.hl1"));
    SuperCompiler0App.main(Array("-si", "tasks/input/01b_plusxyz.hl0", "-t", "tasks/output/01b_plusxyz.svg", "-p", "tasks/output/01b_plusxyz.hl1"));
  }
  
  @Test def task02_map_comp(): Unit = {    
    SuperCompiler0App.main(Array("-si", "tasks/input/02a_map_comp.hl0", "-t", "tasks/output/02a_map_comp.svg", "-p", "tasks/output/02a_map_comp.hl1"));
    SuperCompiler0App.main(Array("-si", "tasks/input/02b_map_comp.hl0", "-t", "tasks/output/02b_map_comp.svg", "-p", "tasks/output/02b_map_comp.hl1"));
  }
  
  @Test def task03_map_join(): Unit = {
    SuperCompiler0App.main(Array("-si", "tasks/input/03a_map_join.hl0", "-t", "tasks/output/03a_map_join.svg", "-p", "tasks/output/03a_map_join.hl1"));
    SuperCompiler0App.main(Array("-si", "tasks/input/03b_map_join.hl0", "-t", "tasks/output/03b_map_join.svg", "-p", "tasks/output/03b_map_join.hl1"));
    
    //SuperCompiler0App.main(Array("-si", "tasks/input/map_join_a1.hl0", "-t", "tasks/output/map_join_a1.svg", "-p", "tasks/output/map_join_a1.hl1"));
    //SuperCompiler0App.main(Array("-si", "tasks/input/map_join_a2.hl0", "-t", "tasks/output/map_join_a2.svg", "-p", "tasks/output/map_join_a2.hl1"));
  }
  
  @Test def task04_app_rev(): Unit = {    
    SuperCompiler0App.main(Array("-si", "tasks/input/04a_app_rev.hl0", "-t", "tasks/output/04a_app_rev.svg", "-p", "tasks/output/04a_app_rev.hl1"));
    SuperCompiler0App.main(Array("-si", "tasks/input/04b_app_rev.hl0", "-t", "tasks/output/04b_app_rev.svg", "-p", "tasks/output/04b_app_rev.hl1"));
  }
  
  @Test def task05_app_rev(): Unit = {    
    SuperCompiler0App.main(Array("-si", "tasks/input/05a_app_rev.hl0", "-t", "tasks/output/05a_app_rev.svg", "-p", "tasks/output/05a_app_rev.hl1"));
    SuperCompiler0App.main(Array("-si", "tasks/input/05b_app_rev.hl0", "-t", "tasks/output/05b_app_rev.svg", "-p", "tasks/output/05b_app_rev.hl1"));
  }
  
  @Test def task06_app_rev(): Unit = {    
    SuperCompiler0App.main(Array("-si", "tasks/input/06a_app_rev.hl0", "-t", "tasks/output/06a_app_rev.svg", "-p", "tasks/output/06a_app_rev.hl1"));
    SuperCompiler0App.main(Array("-si", "tasks/input/06b_app_rev.hl0", "-t", "tasks/output/06b_app_rev.svg", "-p", "tasks/output/06b_app_rev.hl1"));
  }
  
  @Test def task07_eqnum_plus(): Unit = {    
    SuperCompiler0App.main(Array("-si", "tasks/input/07a_eqnum_plus.hl0", "-t", "tasks/output/07a_eqnum_plus.svg", "-p", "tasks/output/07a_eqnum_plus.hl1"));
    //SuperCompiler0App.main(Array("-si", "tasks/input/07b_eqnum_plus.hl0", "-t", "tasks/output/07b_eqnum_plus.svg", "-p", "tasks/output/07b_eqnum_plus.hl1"));
    
    SuperCompiler0App.main(Array("-si", "tasks/input/07c_eqnum_plus.hl0", "-t", "tasks/output/07c_eqnum_plus.svg", "-p", "tasks/output/07c_eqnum_plus.hl1"));
    SuperCompiler0App.main(Array("-si", "tasks/input/07d_eqnum_plus.hl0", "-t", "tasks/output/07d_eqnum_plus.svg", "-p", "tasks/output/07d_eqnum_plus.hl1"));
  }
  
  @Test def task08_eureka(): Unit = {    
    SuperCompiler0App.main(Array("-si", "tasks/input/08a_eureka.hl0", "-t", "tasks/output/08a_eureka.svg", "-p", "tasks/output/08a_eureka.hl1"));
    SuperCompiler0App.main(Array("-si", "tasks/input/08b_eureka.hl0", "-t", "tasks/output/08b_eureka.svg", "-p", "tasks/output/08b_eureka.hl1"));
    SuperCompiler0App.main(Array("-si", "tasks/input/08c_eureka.hl0", "-t", "tasks/output/08c_eureka.svg", "-p", "tasks/output/08c_eureka.hl1"));
  }
  
  @Test def task09(): Unit = {    
    SuperCompiler0App.main(Array("-si", "tasks/input/09a.hl0", "-t", "tasks/output/09a.svg", "-p", "tasks/output/09a.hl1"));
    SuperCompiler0App.main(Array("-si", "tasks/input/09b.hl0", "-t", "tasks/output/09b.svg", "-p", "tasks/output/09b.hl1"));
  }
  
  @Test def task10_len_rev(): Unit = {    
    SuperCompiler0App.main(Array("-si", "tasks/input/10a_len_rev.hl0", "-t", "tasks/output/10a_len_rev.svg", "-p", "tasks/output/10a_len_rev.hl1"));
    //SuperCompiler0App.main(Array("-si", "tasks/input/10b_len_rev.hl0", "-t", "tasks/output/10b_len_rev.svg", "-p", "tasks/output/10b_len_rev.hl1"));
    //SuperCompiler0App.main(Array("-si", "tasks/input/10c_len_rev.hl0", "-t", "tasks/output/10c_len_rev.svg", "-p", "tasks/output/10c_len_rev.hl1"));
  }
  
  @Test def task11_rev(): Unit = {    
    SuperCompiler0App.main(Array("-si", "tasks/input/11a_rev.hl0", "-t", "tasks/output/11a_rev.svg", "-p", "tasks/output/11a_rev.hl1"));
    //SuperCompiler0App.main(Array("-si", "tasks/input/11b_rev.hl0", "-t", "tasks/output/11b_rev.svg", "-p", "tasks/output/11b_rev.hl1"));
  }
  
  @Test def leq_add(): Unit = {    
    SuperCompiler0App.main(Array("-si", "tasks/input/leq_add.hl0", "-t", "tasks/output/leq_add.svg", "-p", "tasks/output/leq_add.hl1"));
  }
  
  @Test def isort(): Unit = {    
    SuperCompiler0App.main(Array("-si", "tasks/input/isort.hl0", "-t", "tasks/output/isort.svg", "-p", "tasks/output/isort.hl1"));
  }
  
  @Test def min(): Unit = {    
    SuperCompiler0App.main(Array("-si", "tasks/input/min.hl0", "-t", "tasks/output/min.svg", "-p", "tasks/output/min.hl1"));
  }
  
}
