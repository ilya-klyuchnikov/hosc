-- generated by hosc0 from debug/m1.hs

data Term a = Lit a | Oper (a->(a->a)) (Term a) (Term a);
data Ctx a = Redex (a->(a->a)) a a | CL (a->(a->a)) (Ctx a) (Term a) | CR (a->(a->a)) a (Ctx a);
data Decomposition a = Val a | Dec (Ctx a);
data Wrapper a = W a;

(letrec
  f=(\r34->
    case  r34  of {
      Lit s5 -> (Lit s5);
      Oper p23 y5 p18 ->
        (f
          (letrec
            g=(\s34->
              (\t34->
                (\x35->
                  case  s34  of {
                    Lit s31 ->
                      case  t34  of {
                        Lit u23 -> (Lit ((x35 s31) u23));
                        Oper t11 y29 z7 -> (Oper x35 (Lit s31) (((g y29) z7) t11));
                      };
                    Oper p24 r5 u21 -> (Oper x35 (((g r5) u21) p24) t34);
                  })))
          in
            (((g y5) p18) p23)));
    })
in
  (f expr))
