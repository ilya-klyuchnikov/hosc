package hosc

import scala.util.parsing.input.Positional

import HLanguage._
import HParsers._

object Validator {
  case class ValidatorError(error: Error) extends Exception(error.toString) {}
  def validate(s: Success[Program]): ParseResult[Program] = {
    val p = s.get
    var typeNames = Set[String]()
    var dataConNames = Set[String]()
    val fNames = Set[String]()
    val fNamesInDefs = Set.empty[String] ++ (p.fs map (_.name))

    def valTD(td: TypeDefinition): Unit = {
      var tvs = Set.empty[TypeVariable]
      var tvsUsed = Set.empty[TypeVariable]

      def valTypeUsage(t: Type, allowFreeVars: Boolean): Unit = t match {
        case tv: TypeVariable =>
          if (!allowFreeVars && !(tvs contains tv)) err("undefined var " + tv.name)
          tvsUsed += tv
        case Arrow(t1, t2) => valTypeUsage(t1, allowFreeVars); valTypeUsage(t2, allowFreeVars);
        case TypeConstructor(n, args) =>
          p.getTypeConstructorDefinition(n) match {
            case Some(td) =>
              if (td.args.length != args.length) err("wrong numbers of argument for type " + n)
              for (a <- args) valTypeUsage(a, allowFreeVars)
            case None =>
              err("unknown type " + n)
          }
      }

      def valDC(dc: DataConstructor): Unit = {
        if (dataConNames contains dc.name) err("duplicate data constructor definition " + dc.name)
        dataConNames += dc.name
        for (a <- dc.args) valTypeUsage(a, false)
      }

      if (typeNames contains td.name) err("duplicate type name " + td.name)
      typeNames += td.name

      td match {
        case tcd: TypeConstructorDefinition =>
          for (v <- tcd.args) {
            if (tvs contains v) err("duplicate type variable " + v.name)
            tvs += v
          }
          for (dc <- tcd.cons) valDC(dc)
          tcd.args find { !tvsUsed.contains(_) } match {
            case Some(tv) => err("useless type variable " + tv.name)
            case None     =>
          }
      }
    }

    def valFD(f: Function): Unit = {
      if (fNames contains f.name) err("duplicate function " + f.name, f)
      valTerm(fNamesInDefs, f.body, p)
    }

    try {
      for (td <- p.ts) valTD(td)
      for (f <- p.fs) valFD(f)
      valTermWithFreeVars(fNamesInDefs, p.goal, p)
      s
    } catch {
      case ValidatorError(he) => he
    }
  }

  private def err(msg: String, pos: Positional) = {
    throw ValidatorError(sys.error(msg))
  }

  private def err(msg: String) = {
    throw ValidatorError(sys.error(msg))
  }

  def valTerm(boundedVars: Set[String], term: Expression, p: Program): Unit = term match {
    case v: Variable =>
      if (!(boundedVars contains v.name)) err("unbounded variable " + v.name, v)
    case c: Constructor =>
      p.getDataConstructor(c.name) match {
        case Some(dc) =>
          if (dc.args.length != c.args.length) err("wrong number of parameters for constructor " + c.name, c)
          for (arg <- c.args) valTerm(boundedVars, arg, p)
        case None => err("undefined constructor " + c.name, c)
      }
    case l: LambdaAbstraction =>
      if (boundedVars contains l.v.name) err("variable " + l.v.name + " is already bound", l.v)
      valTerm(boundedVars + l.v.name, l.t, p)
    case a: Application =>
      valTerm(boundedVars, a.head, p)
      valTerm(boundedVars, a.arg, p)
    case c: CaseExpression =>
      valCase(boundedVars, c, p)
    case let: LetExpression =>
      err("unexpected let", let)
    case letrec: LetRecExpression =>
      err("unexpected letrec", letrec)
  }

  private def valCase(boundedVars: Set[String], c: CaseExpression, p: Program): Unit = {
    valTerm(boundedVars, c.selector, p)
    if (c.branches.isEmpty) {
      return
    }
    val pat = c.branches.head.pattern
    val dcn = pat.name
    p.getTypeDefinitionForDC(dcn) match {
      case None => err("undefined constructor " + dcn, pat)
      case Some(td) =>
        val consNames = Set.empty[String] ++ (td.cons map (_.name))
        var usedNames = Set.empty[String]
        for (b <- c.branches) {
          val pt = b.pattern
          if (!(consNames contains pt.name)) err("type " + td.name + " doesn't define constructor " + pt.name, pt)
          val dc = p.getDataConstructor(pt.name).get
          if (dc.args.length != pt.args.length) err("wrong number of parameters for constructor " + pt.name, pt)
          if (usedNames contains dc.name) err("duplicate pattern " + pt.name, pt)
          usedNames += pt.name
          var pVars = Set.empty[String]
          for (v <- pt.args) {
            if (boundedVars contains v.name) err("variable " + v.name + " is already bound", v)
            if (pVars contains v.name) err("duplicate variable " + v.name + " in pattern", v)
            pVars += v.name
          }
          valTerm(boundedVars ++ pVars, b.term, p)
        }
      //val unused = consNames -- usedNames
      //if (!(unused isEmpty)) err("case is not exhaustive. missing pattern(s) " + unused.mkString(", "), c.selector)
    }
  }

  def valTermWithFreeVars(boundedVars: Set[String], term: Expression, p: Program): Unit = term match {
    case v: Variable =>
    case c: Constructor =>
      p.getDataConstructor(c.name) match {
        case Some(dc) =>
          if (dc.args.length != c.args.length) err("wrong number of parameters for constructor " + c.name, c)
          for (arg <- c.args) valTermWithFreeVars(boundedVars, arg, p)
        case None => err("undefined constructor " + c.name, c)
      }
    case l: LambdaAbstraction =>
      if (boundedVars contains l.v.name) err("variable " + l.v.name + " is already bound", l.v)
      valTermWithFreeVars(boundedVars + l.v.name, l.t, p)
    case a: Application =>
      valTermWithFreeVars(boundedVars, a.head, p);
      valTermWithFreeVars(boundedVars, a.arg, p);
    case c: CaseExpression => valCaseWithFreeVars(boundedVars, c, p)
    case LetRecExpression((v, e), e0) =>
      valTermWithFreeVars(boundedVars, e, p);
      valTermWithFreeVars(boundedVars, e0, p)
    case let: LetExpression => err("unexpected let-expression", let)
  }

  private def valCaseWithFreeVars(boundedVars: Set[String], c: CaseExpression, p: Program): Unit = {
    valTermWithFreeVars(boundedVars, c.selector, p);
    if (c.branches.isEmpty) {
      return
    }
    val pat = c.branches.head.pattern
    val dcn = pat.name
    p.getTypeDefinitionForDC(dcn) match {
      case None => err("undefined constructor " + dcn, pat)
      case Some(td) =>
        val consNames = Set.empty[String] ++ (td.cons map (_.name))
        var usedNames = Set.empty[String]
        for (b <- c.branches) {
          val pt = b.pattern
          if (!(consNames contains pt.name)) err("type " + td.name + " doesn't define constructor " + pt.name, pt)
          val dc = p.getDataConstructor(pt.name).get
          if (dc.args.length != pt.args.length) err("wrong number of parameters for constructor " + pt.name, pt)
          if (usedNames contains dc.name) err("duplicate pattern " + pt.name, pt)
          usedNames += pt.name
          var pVars = Set.empty[String]
          for (v <- pt.args) {
            if (boundedVars contains v.name) err("variable " + v.name + " is already bound", v)
            if (pVars contains v.name) err("duplicate variable " + v.name + " in pattern", v)
            pVars += v.name
          }
          valTermWithFreeVars(boundedVars ++ pVars, b.term, p)
        }
      //val unused = consNames -- usedNames
      // TODO: warning
      //if (!(unused isEmpty)) err("case is not exhaustive. missing pattern(s) " + unused.mkString(", "), c.selector)
    }
  }

}
