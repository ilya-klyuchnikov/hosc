package hosc

import HLanguage._
import HParsers._

object Validator {
  case class ValidatorError(error: HError) extends Exception {}
  def validate(s: Success[Program]) =  {
    val p = s.get
    var typeNames = Set.empty[String]
    var dataConNames = Set.empty[String]
    
    def valTD(td: TypeDefinition) = {
      var tvs = Set.empty[TypeVariable]
      var tvsUsed = Set.empty[TypeVariable]
      
      def valTypeUsage(t: Type): Unit = t match {
        case tv: TypeVariable => {
          if (!(tvs contains tv)) throw ValidatorError(error("undefined var " + tv.name, tv))
          tvsUsed += tv
        }
        case Arrow(t1, t2) => valTypeUsage(t1); valTypeUsage(t2); 
        case tc @ TypeConstructor(n, args) => {
          p.getTypeConstructorDefinition(n) match {
            case Some(td) => {
              if (td.args.length != args.length) 
                throw ValidatorError(error("wrong numbers of argument for type " + n, tc))
              for (a <- args) valTypeUsage(a)
            }
            case None => throw ValidatorError(error("unknown type " + n, tc))
          }
        }
      }
      
      def valDC(dc: DataConstructor) = {
        if (dataConNames contains dc.name) throw ValidatorError(error("duplicate data constructor definition " + dc.name, dc))
        dataConNames += dc.name
        for (a <- dc.args) valTypeUsage(a)
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
        case ad: ArrowDefinition =>      
      }
    }
    
    
    def valFD(f: Function) = {
      
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
