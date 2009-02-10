package bootstrap.liftweb

import net.liftweb.util.Full
import net.liftweb.http.{ResponseInfo, LiftRules, S}
import net.liftweb.sitemap.{Menu, SiteMap, Loc}
import net.liftweb.sitemap.Loc._
 
/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  
  val xhtmlSvgMathMl = """<?xml-stylesheet type="text/xsl" href="dummy.xsl"?>
  <!DOCTYPE html PUBLIC 
  "-//W3C//DTD XHTML 1.1 plus MathML 2.0 plus SVG 1.1//EN" 
  "http://www.w3.org/2002/04/xhtml-math-svg/xhtml-math-svg.dtd">"""
  def boot {
    
    ResponseInfo.docType = {
      case _ if S.getDocType._1 => S.getDocType._2
      case _ => Full(xhtmlSvgMathMl)
    }
    
    // where to search snippet
    LiftRules.addToPackages("hosc")     

    // Build SiteMap
    val entries = Menu(Loc("Home", "index" :: Nil, "Home")) :: 
                  Menu(Loc("sc0", "sc0" :: Nil, "SC0")) ::
                  Menu(Loc("sc0_result", "sc0_result" :: Nil, "sc0_result", Hidden)) ::
                  Menu(Loc("sc1", "sc1" :: Nil, "SC1")) ::
                  Menu(Loc("sc1_result", "sc1_result" :: Nil, "sc1_result", Hidden)) ::
                  Nil 
    LiftRules.setSiteMap(SiteMap(entries:_*))
    
    // web-service for appengine
    LiftRules.dispatch.append(hosc.service.AppService.dispatcher)
  }
}

