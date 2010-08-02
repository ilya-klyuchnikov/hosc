package hosc

import hosc.{SuperCompilerApp => SCP0, SCLemApp}
import hosc.exp.{SuperCompilerApp1 => SCP1, SuperCompilerApp2 => SCP2}

object Protocols {
  def main(args : Array[String]) : Unit = {
    //SCP1.main(Array("-si", "protocols/01_synapse.hs",  "-t", "protocols/out/01_synapse.svg", "-p", "protocols/out/01_synapse.hs"))
    //SCP0.main(Array("-si", "protocols/02_msi.hs", "-t", "protocols/out/02_msi.svg", "-p", "protocols/out/02_msi.hs"))
    //SCLemApp.main(Array("-si", "protocols/03_mosi.hs",  "-t", "protocols/out/03_mosi.svg", "-p", "protocols/out/03_mosi.hs"))
    //SCLemApp.main(Array("-si", "protocols/03_mosi.hs", "-li", "protocols/lemmas.hs", "-t", "protocols/out/03_mosi.svg", "-p", "protocols/out/03_mosi.hs"))
    //SCP0.main(Array("-si", "protocols/04_mesi.hs", "-t", "protocols/out/04_mesi.svg", "-p", "protocols/out/04_mesi.hs"))
    //SCP0.main(Array("-si", "protocols/05_moesi.hs", "-t", "protocols/out/05_moesi.svg", "-p", "protocols/out/05_moesi.hs"))
    //SCP0.main(Array("-si", "protocols/06_illinois.hs", "-t", "protocols/out/06_illinois.svg", "-p", "protocols/out/06_illinois.hs"))
    SCLemApp.main(Array("-si", "protocols/06_illinois.hs", "-li", "protocols/lemmas.hs", "-t", "protocols/out/06_illinois.svg", "-p", "protocols/out/06_illinois.hs"))
    //SCP0.main(Array("-si", "protocols/07_berkeley.hs", "-t", "protocols/out/07_berkeley.svg", "-p", "protocols/out/07_berkeley.hs"))
    //SCP0.main(Array("-si", "protocols/08_dec.hs", "-t", "protocols/out/08_dec.svg", "-p", "protocols/out/08_dec.hs"))
    //SCLemApp.main(Array("-si", "protocols/08_dec.hs", "-li", "protocols/lemmas.hs", "-t", "protocols/out/08_dec.svg", "-p", "protocols/out/08_dec.hs"))
    //SCP0.main(Array("-si", "protocols/09_futurebus.hs", "-t", "protocols/out/09_futurebus.svg", "-p", "protocols/out/09_futurebus.hs"))
    //SCLemApp.main(Array("-si", "protocols/09_futurebus.hs", "-li", "protocols/lemmas.hs", "-t", "protocols/out/09_futurebus.svg", "-p", "protocols/out/09_futurebus.hs"))
    //SCP0.main(Array("-si", "protocols/10_xerox.hs", "-t", "protocols/out/10_xerox.svg", "-p", "protocols/out/10_xerox.hs"))
    //SCLemApp.main(Array("-si", "protocols/10_xerox.hs", "-li", "protocols/lemmas.hs", "-t", "protocols/out/10_xerox.svg", "-p", "protocols/out/10_xerox.hs"))
    //SCP0.main(Array("-si", "protocols/11_java.hs", "-t", "protocols/out/11_java.svg", "-p", "protocols/out/11_java.hs"))
    //SCP0.main(Array("-si", "protocols/12_reader_writer.hs", "-t", "protocols/out/12_reader_writer.svg", "-p", "protocols/out/12_reader_writer.hs"))
    //SCP0.main(Array("-si", "protocols/13_german_i.hs", "-t", "protocols/out/13_german_i.svg", "-p", "protocols/out/13_german_i.hs"))
    //SCLemApp.main(Array("-si", "protocols/13_german_i.hs", "-li", "protocols/lemmas.hs", "-t", "protocols/out/13_german_i.svg", "-p", "protocols/out/13_german_i.hs"))
    //SCP0.main(Array("-si", "protocols/14_german_b.hs", "-t", "protocols/out/14_german_b.svg", "-p", "protocols/out/14_german_b.hs"))
    //SCLemApp.main(Array("-si", "protocols/14_german_b.hs", "-li", "protocols/lemmas.hs", "-t", "protocols/out/14_german_b.svg", "-p", "protocols/out/14_german_b.hs"))
    //SCP0.main(Array("-si", "protocols/15_data_race.hs", "-t", "protocols/out/15_data_race.svg", "-p", "protocols/out/15_data_race.hs"))
  }
}