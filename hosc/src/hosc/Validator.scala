package hosc

import scala.util.parsing.input.Positional

import HLanguage._
import HParsers._

object Validator {
  case class ValidatorError(error: HError) extends Exception {}
  def validate(s: Success[Program]) =  {
    val p = s.get
    var typeNames = Set.empty[String]
    var dataConNames = Set.empty[String]
    var fNames = Set.empty[String]
    var allFNames = Set.empty[String] ++ (p.fs map (_.name))
    
    def valTD(td: TypeDefinition) = {
      var tvs = Set.empty[TypeVariable]
      var tvsUsed = Set.empty[TypeVariable]
      
      def valTypeUsage(t: Type, allowFreeVars: Boolean): Unit = t match {
        case tv: TypeVariable => {
          if (!allowFreeVars && !(tvs contains tv)) err("undefined var " + tv.name, tv)
          tvsUsed += tv
        }
        case Arrow(t1, t2) => valTypeUsage(t1, allowFreeVars); valTypeUsage(t2, allowFreeVars); 
        case tc @ TypeConstructor(n, args) => {
          p.getTypeConstructorDefinition(n) match {
            case Some(td) => {
              if (td.args.length != args.length) err("wrong numbers of argument for type " + n, tc)
              for (a <- args) valTypeUsage(a, allowFreeVars)
            }
            case None => err("unknown type " + n, tc)
          }
        }
      }
      
      def valDC(dc: DataConstructor) = {
        if (dataConNames contains dc.name) err("duplicate data constructor definition " + dc.name, dc)
        dataConNames += dc.name
        for (a <- dc.args) valTypeUsage(a, false)
      }
      
      if (typeNames contains td.name) err("duplicate type name " + td.name, td)
      typeNames += td.name
      
      td match {
        case tcd: TypeConstructorDefinition => {
          for (v <- tcd.args) {
            if (tvs contains v) err("duplicate type variable " + v.name, v)
            tvs += v
          }
          for (dc <- tcd.cons) valDC(dc)
          tcd.args find {!tvsUsed.contains(_)} match {
            case Some(tv) => err("useless type variable " + tv.name, tv)
            case None =>
          }
        }
        case ad: ArrowDefinition => valTypeUsage(ad.ac, true)  
      }
    }
    
    def valCase(boundedVars: Set[String], c: CaseExpression): Unit = {
      valTerm(boundedVars, c.selector);
      val pat = c.branches.head.pattern
      val dcn = pat.name
      p.getTypeDefinitionForDC(dcn) match {
        case None => err("undefined constructor " + dcn, pat)
        case Some(td) => {
          val consNames = Set.empty[String] ++ (td.cons map (_.name))
          var usedNames = Set.empty[String]
          for (b <- c.branches){
            val pt = b.pattern
            if (!(consNames contains pt.name)) err("type " + td.name +" doesn't define constructor " + pt.name, pt)
            val dc = p.getDataConstructor(pt.name).get
            if (dc.args.length != pt.args.length) err("wrong number of parameters for constructor " + pt.name, pt)
            if (usedNames contains dc.name) err("duplicate " + pt.name, pt)
            usedNames += pt.name
            var pVars = Set.empty[String]
            for (v <- pt.args){
              if (pVars contains v.name) err("duplicate variable " + v.name + " in pattern", v)
              pVars += v.name
            }
            valTerm(boundedVars ++ pVars, b.term)
          }
          val unused = consNames -- usedNames
          if (!(unused isEmpty)) err("case is not exhaustive. missing pattern(s) " + unused.mkString(", "), c.selector)
        }
      }
    }
    
    def valTerm(boundedVars: Set[String], term: Term): Unit = term match{
      case v: Variable => 
        if (!(boundedVars contains v.name)) err("unbounded variable " + v.name, v)
      case c: Constructor => {
        p.getDataConstructor(c.name) match {
          case Some(dc) => {
            if (dc.args.length != c.args.length) err("wrong number of parameters for constructor " + c.name, c)
            for (arg <- c.args) valTerm(boundedVars, arg)
          }
          case None => err("undefined constructor " + c.name, c)
        }
      }
      case l: LambdaAbstraction => valTerm(boundedVars + l.v.name, l.t)
      case a: Application => {valTerm(boundedVars, a.head); valTerm(boundedVars, a.arg);}
      case c: CaseExpression => valCase(boundedVars, c)
    }    
    
    def valFD(f: Function) = {
      if (fNames contains f.name) err("duplicate function " + f.name, f)
      valTerm(allFNames, f.lam)
    }
    
    try {     
      for(td <- p.ts) valTD(td)
      for(f <- p.fs) valFD(f)
      s
    } catch {
      case ValidatorError(he) => he
    }
  }
  
  def err(msg: String, pos: Positional) = {
    throw ValidatorError(error(msg, pos))
  }
  
}
