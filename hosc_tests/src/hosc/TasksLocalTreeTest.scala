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
}
