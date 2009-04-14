package hosc

import org.junit.Test
import org.junit.Ignore

class SuperCompiler0Test {
  @Test def app2(): Unit ={    
    SuperCompilerApp.main(Array("-si", "hl0/2app.hl0",
        "-t", "output/hl0/2app.svg",
        "-p", "output/hl0/2app.hl1"));
  }

  @Test def rev(): Unit ={    
    SuperCompilerApp.main(Array("-si", "hl0/rev.hl0",
        "-t", "output/hl0/rev.svg",
        "-p", "output/hl0/rev.hl1"));
  }
  
  @Test def app_rev1(): Unit ={    
    SuperCompilerApp.main(Array("-si", "hl0/app_rev1.hl0",
        "-t", "output/hl0/app_rev1.svg",
        "-p", "output/hl0/app_rev1.hl1"));
  }
  
  @Test def app_rev2(): Unit ={    
    SuperCompilerApp.main(Array("-si", "hl0/app_rev2.hl0",
        "-t", "output/hl0/app_rev2.svg",
        "-p", "output/hl0/app_rev2.hl1"));
    
  
  }
  
  @Test def app_rev3(): Unit ={    
    SuperCompilerApp.main(Array("-si", "hl0/app_rev3.hl0",
        "-t", "output/hl0/app_rev3.svg",
        "-p", "output/hl0/app_rev3.hl1"));
  }
  
  @Test def fib(): Unit ={    
    SuperCompilerApp.main(Array("-si", "hl0/fib.hl0",
        "-t", "output/hl0/fib.svg",
        "-p", "output/hl0/fib.hl1"));
  }
  
}
