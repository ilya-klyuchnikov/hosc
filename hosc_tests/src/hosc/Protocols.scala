package hosc

import hosc.{SuperCompilerApp => SCP0}
import hosc.exp.{SuperCompilerApp1 => SCP1, SuperCompilerApp2 => SCP2}

object Protocols {
  def main(args : Array[String]) : Unit = {
    SCP1.main(Array("-si", "protocols/01_synapse.hs", 
                    "-t", "protocols/out/01_synapse.svg",
                    "-p", "protocols/out/01_synapse.hs"))
     
    SCP0.main(Array("-si", "protocols/15_data_race.hs", 
                    "-t", "protocols/out/15_data_race.svg",
                    "-p", "protocols/out/15_data_race.hs"))
    
    SCP0.main(Array("-si", "protocols/02_msi.hs", 
                    "-t", "protocols/out/02_msi.svg",
                    "-p", "protocols/out/02_msi.hs"))
    SCP0.main(Array("-si", "protocols/03_mosi.hs", 
                    "-t", "protocols/out/03_mosi.svg",
                    "-p", "protocols/out/03_mosi.hs"))
  }
}
