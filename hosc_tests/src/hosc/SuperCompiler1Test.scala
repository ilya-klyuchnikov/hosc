package hosc;
import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import HLanguage._
import Util._
class SuperCompiler1Test {
  @Test def sc01(): Unit ={    
    SuperCompilerApp1.main(Array("-si", "hl/sc1/sc01.hl",
        "-t", "output/sc01.svg",
        "-p", "output/sc01.hl"));
  }
  
  @Test def sc_even(): Unit ={    
    SuperCompilerApp1.main(Array("-si", "hl/sc1/sc_even.hl",
        "-t", "output/sc_even.svg",
        "-p", "output/sc_even.hl"));
  }
  
  @Test def sc_even_plus(): Unit ={    
    SuperCompilerApp1.main(Array("-si", "hl/sc1/sc_even_plus.hl",
        "-t", "output/sc_even_plus.svg",
        "-p", "output/sc_even_plus.hl"));
  }
  
  @Test def sc_rev(): Unit ={    
    SuperCompilerApp1.main(Array("-si", "hl/sc1/sc_rev.hl",
        "-t", "output/sc_rev.svg",
        "-p", "output/sc_rev.hl"));
  }
  
  @Test def sc_eqnum_plus(): Unit ={    
    SuperCompilerApp1.main(Array("-si", "hl/sc1/sc_eqnum_plus.hl",
        "-t", "output/sc_eqnum_plus.svg",
        "-p", "output/sc_eqnum_plus.hl"));
  }
  @Test def sc_eqnum_plus1(): Unit ={    
    SuperCompilerApp1.main(Array("-si", "hl/sc1/sc_eqnum_plus1.hl",
        "-t", "output/sc_eqnum_plus1.svg",
        "-p", "output/sc_eqnum_plus1.hl"));
  }
}
