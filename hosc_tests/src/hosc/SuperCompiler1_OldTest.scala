package hosc;
import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import HLanguage._
import Util._
class SuperCompiler1_OldTest {
  @Test def leqxplusxy(): Unit ={    
    SuperCompilerApp1.main(Array("-si", "hl/sc1/sc_leqxplusxy.hl",
        "-t", "output/sc_leqxplusxy.svg",
        "-p", "output/sc_leqxplusxy.hl"));
  }
  @Test{val timeout = 20000}
  def sc_even(): Unit ={    
    SuperCompilerApp1.main(Array("-si", "hl/sc1/sc_even.hl",
        "-t", "output/sc_even.svg",
        "-p", "output/sc_even.hl"));
  }
  
  @Test def sc_even_plus(): Unit ={    
    SuperCompilerApp1.main(Array("-si", "hl/sc1/sc_even_plus.hl",
        "-t", "output/sc_even_plus.svg",
        "-p", "output/sc_even_plus.hl"));
  }
  
  @Test def sc_leq_plusr(): Unit ={    
    SuperCompilerApp1.main(Array("-si", "hl/sc1/sc_leqplus.hl",
        "-t", "output/sc_leqplus.svg",
        "-p", "output/sc_leqplus.hl"));
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
  
  @Test def sc_synapse(): Unit ={    
    SuperCompilerApp1.main(Array("-si", "hl/sc1/sc_synapse.hl",
        "-t", "output/sc_synapse.svg",
        "-p", "output/sc_synapse.hl"));
  }
  
  @Test{val timeout = 10000}
  def sc_exp(): Unit ={    
    SuperCompilerApp1.main(Array("-si", "hl/sc1/sc_exp.hl",
        "-t", "output/sc_exp.svg",
        "-p", "output/sc_exp.hl"));
  }
}
