package hosc

object TypeAlgebra {
  private var n: Int = 0
  def newTyvar(): TypeVariable = { n += 1; TypeVariable("$$" + n) }
  
  def tyvars(t: Type): List[TypeVariable] = t match {
    case tv @ TypeVariable(a) => List(tv)
    case Arrow(t1, t2) => tyvars(t1) union tyvars(t2)
    case TypeConstructor(k, ts) => (List[TypeVariable]() /: ts) ((tvs, t) => tvs union tyvars(t))
  }
  
  def equivalent(type1: Type, type2: Type): Boolean = {
    val map1to2 = scala.collection.mutable.Map[TypeVariable, TypeVariable]()
    val map2to1 = scala.collection.mutable.Map[TypeVariable, TypeVariable]()
    def eq1(t1: Type, t2: Type): Boolean = (t1, t2) match {
      case (v1: TypeVariable, v2: TypeVariable) => (map1to2.get(v1), map2to1.get(v2)) match {
        case (Some(v3), Some(v4)) => v2 == v3 && v1 == v4
        case (None, None) => map1to2(v1) = v2; map2to1(v2) = v1; true
        case _ => false
      }
      case (TypeConstructor(name1, args1), TypeConstructor(name2, args2)) if name1 == name2 =>
        ((args1 zip args2) forall (args => eq1(args._1, args._2)))
      case (Arrow(b1, v1), Arrow(b2, v2)) =>
        eq1(b1, b2) && eq1(v1, v2)
      case _ => 
        false
    }    
    eq1(type1, type2)
  }
}
