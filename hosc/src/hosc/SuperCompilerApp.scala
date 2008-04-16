package hosc;

import scala.util.parsing.input.StreamReader
import scala.util.parsing.input.CharArrayReader

import java.io.File
import java.io.FileInputStream
import java.io.InputStreamReader
import java.io.FileReader
import java.io.FileWriter
import java.io.BufferedReader

import HLanguage._
import HParsers._
import Util._

object SuperCompilerApp {
  val help = """usage: hosc.SuperCompilerApp -i input_file -e expression -t tree_output_file -p program_output_file
  |Where:
  |input_file            path to input file (relative or absolute)
  |expression            term to be supercompiled
  |tree_output_file      path to file where process tree will be placed (in SVG format)
  |program_output_file   path to file where residual program will be placed
  |""".stripMargin
  def main(args : Array[String]) : Unit = {
    var fileName: String = null
    var termString: String = null
    var outFileName: String = null
    var outProgramFileName: String = null
    args.toList match {
      case "-i" :: input_file :: "-e" :: expr :: "-t" :: output_file :: "-p" :: output_file_1 :: Nil =>
        fileName = input_file
        termString = expr
        outFileName = output_file
        outProgramFileName = output_file_1
      case "-help" :: Nil => 
        println(help)
        return
      case _ => 
        throw new IllegalArgumentException("run spcs.SuperCompilerApp -help for help")       
    }
    val program = programFromFile(fileName)
    val sc = new SuperCompiler(program)
    val term = termFromString(termString, program)
    val pt = sc.buildProcessTree(term)    
    val svg = ProcessTreeSVG.treeToSVG(pt)
    
    val svgFile = new java.io.File(outFileName)
    if (!svgFile.exists){
      svgFile.createNewFile()
    } 
    scala.xml.XML.save(outFileName, svg)
    /*
    val text = generateResidualProgram(pt).toString
    val slFile = new java.io.File(outProgramFileName)
    if (!slFile.exists){
      slFile.createNewFile()
    }
    val fw = new FileWriter(slFile);
    fw.write(text);
    fw.flush();
    fw.close();
    */
  }
}
