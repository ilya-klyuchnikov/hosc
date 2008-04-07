package hosc

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
          if (!allowFreeVars && !(tvs contains tv)) throw ValidatorError(error("undefined var " + tv.name, tv))
          tvsUsed += tv
        }
        case Arrow(t1, t2) => valTypeUsage(t1, allowFreeVars); valTypeUsage(t2, allowFreeVars); 
        case tc @ TypeConstructor(n, args) => {
          p.getTypeConstructorDefinition(n) match {
            case Some(td) => {
              if (td.args.length != args.length) 
                throw ValidatorError(error("wrong numbers of argument for type " + n, tc))
              for (a <- args) valTypeUsage(a, allowFreeVars)
            }
            case None => throw ValidatorError(error("unknown type " + n, tc))
          }
        }
      }
      
      def valDC(dc: DataConstructor) = {
        if (dataConNames contains dc.name) 
          throw ValidatorError(error("duplicate data constructor definition " + dc.name, dc))
        dataConNames += dc.name
        for (a <- dc.args) valTypeUsage(a, false)
      }
      
      if (typeNames contains td.name) throw ValidatorError(error("duplicate type name " + td.name, td))
      typeNames += td.name
      
      td match {
        case tcd: TypeConstructorDefinition => {
          for (v <- tcd.args) {
            if (tvs contains v) throw ValidatorError(error("duplicate type variable " + v.name, v))
            tvs += v
          }
          for (dc <- tcd.cons) valDC(dc)
          tcd.args find {!tvsUsed.contains(_)} match {
            case Some(tv) => throw ValidatorError(error("useless type variable " + tv.name, tv))
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
        case None => throw ValidatorError(error("undefined constructor " + dcn, pat))
        case Some(td) => {
          val consNames = Set.empty[String] ++ (td.cons map (_.name))
          var usedNames = Set.empty[String]
          for (b <- c.branches){
            val pattern = b.pattern
            if (!(consNames contains pattern.name))
              throw ValidatorError(error("type " + td.name +" doesn't define constructor " + pattern.name, pattern))
            val dc = p.getDataConstructor(pattern.name).get
            if (dc.args.length != pattern.args.length) 
              throw ValidatorError(error("wrong number of parameters for constructor " + pattern.name, pattern))
            if (usedNames contains dc.name)
              throw ValidatorError(error("duplicate " + pattern.name, pattern))
            usedNames += pattern.name
            var pVars = Set.empty[String]
            for (v <- pattern.args){
              if (pVars contains v.name)
                throw ValidatorError(error("duplicate variable " + v.name + " in pattern", v))
              pVars += v.name
            }
            valTerm(boundedVars ++ pVars, b.term)
          }
          val unused = consNames -- usedNames
          if (!(unused isEmpty)) {
            throw ValidatorError(error("case is not exghaustive. missing patterns " + unused.mkString(", "), c.selector))
          }
        }
      }
    }
    
    def valTerm(boundedVars: Set[String], term: Term): Unit = term match{
      case v: Variable => 
        if (!(boundedVars contains v.name)) throw ValidatorError(error("unbounded variable " + v.name, v))
      case c: Constructor => {
        p.getDataConstructor(c.name) match {
          case Some(dc) => {
            if (dc.args.length != c.args.length) 
              throw ValidatorError(error("wrong number of parameters for constructor " + c.name, c))
            for (arg <- c.args)
              valTerm(boundedVars, arg)
          }
          case None => throw ValidatorError(error("undefined constructor " + c.name, c))
        }
        null
      }
      case l: LambdaAbstraction => valTerm(boundedVars + l.v.name, l.t)
      case a: Application => {valTerm(boundedVars, a.head); valTerm(boundedVars, a.arg);}
      case c: CaseExpression => valCase(boundedVars, c)
    }
    
    
    def valFD(f: Function) = {
      if (fNames contains f.name) throw ValidatorError(error("duplicate function " + f.name, f))
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
  
}
