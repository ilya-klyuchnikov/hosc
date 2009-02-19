package hosc

import org.junit.Test
import org.junit.Ignore
import sc0.LocalTreeBuilder0App

class TasksLocalTreeTest {
  @Test def task01_eqnum_plus(): Unit = {    
    LocalTreeBuilder0App.main(Array("-i", "tasks_lt/input/01a_eqnum_plus.hl0", "-t", "tasks_lt/output/01a_eqnum_plus.svg"));
    LocalTreeBuilder0App.main(Array("-i", "tasks_lt/input/01b_eqnum_plus.hl0", "-t", "tasks_lt/output/01b_eqnum_plus.svg"));
    LocalTreeBuilder0App.main(Array("-i", "tasks_lt/input/01c_eqnum_plus.hl0", "-t", "tasks_lt/output/01c_eqnum_plus.svg"));
  }
  
  @Test def task02_app_rev(): Unit = {    
    LocalTreeBuilder0App.main(Array("-i", "tasks_lt/input/02a_app_rev.hl0", "-t", "tasks_lt/output/02a_app_rev.svg"));
  }
  
  @Test def task03_app(): Unit = {    
    LocalTreeBuilder0App.main(Array("-i", "tasks_lt/input/03a_app.hl0", "-t", "tasks_lt/output/03a_app.svg"));
  }
  
  @Test def task04_plusxx(): Unit = {    
    LocalTreeBuilder0App.main(Array("-i", "tasks_lt/input/04a_plusxx.hl0", "-t", "tasks_lt/output/04a_plusxx.svg"));
    LocalTreeBuilder0App.main(Array("-i", "tasks_lt/input/04b_plusxyz.hl0", "-t", "tasks_lt/output/04b_plusxyz.svg"));
  }
  
  @Test def task05_map_compose(): Unit = {    
    LocalTreeBuilder0App.main(Array("-i", "tasks_lt/input/05a_map_compose.hl0", "-t", "tasks_lt/output/05a_map_compose.svg"));
    LocalTreeBuilder0App.main(Array("-i", "tasks_lt/input/05b_map_compose.hl0", "-t", "tasks_lt/output/05b_map_compose.svg"));
  }
}
