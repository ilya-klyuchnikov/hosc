package hosc.sc0

import scala.util.parsing.input.StreamReader
import scala.util.parsing.input.CharArrayReader

import java.io.File
import java.io.FileInputStream
import java.io.InputStreamReader
import java.io.FileReader
import java.io.FileWriter
import java.io.BufferedReader

import HLanguage._
import Util._

object LocalTreeBuilder0App {
  val help = """usage: hosc.LocalTreeBuilder0App [-i input_file] -t tree_output_file
  |Where:
  |input_file           path to input file (code in hl0 language)
  |tree_output_file      path to file where process tree will be placed (in SVG format)
  |""".stripMargin
  def main(args : Array[String]) : Unit = {
    var fileName: String = null
    var outFileName: String = null
    var sugared = false
    args.toList match {
      case "-i" :: input_file :: "-t" :: output_file :: Nil =>
        fileName = input_file
        outFileName = output_file
      case "-help" :: Nil => 
        println(help)
        return
      case _ => 
        throw new IllegalArgumentException("run spcs.SuperCompilerApp -help for help")       
    }
    
    val program = programFromFile(fileName)
    val sc = new LocalTreeBuilder0(program)
    val pt = sc.buildProcessTree(program.goal)    
    val svg = new ProcessTree0SVG(pt).treeToSVG
    
    val svgFile = new java.io.File(outFileName)
    if (!svgFile.exists){
      svgFile.createNewFile()
    } 
    scala.xml.XML.save(outFileName, svg)  
  }
}
