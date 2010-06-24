-- generated by hosc0 from debug/add_machine.hs

data Term a = Lit a | Add (Term a) (Term a);
data Red a = Sum a a;
data Context a = Hole (Red a) | C1 (Context a) (Term a) | C2 a (Context a);
data Dec a = Val a | Ctx (Context a);

(letrec
  g=(\z15->
    case  z15  of {
      Lit r -> r;
      Add x z1 ->
        case 
        (letrec
          h=(\u15->
            (\v15->
              case  u15  of {
                Lit v14 ->
                  case  v15  of {
                    Lit w1 -> (Ctx (Hole (Sum v14 w1)));
                    Add r1 y ->
                      case  ((h r1) y)  of { Ctx w12 -> (Ctx (C2 v14 w12)); Val p -> (Ctx (Hole (Sum v14 p))); };
                  };
                Add z12 s12 ->
                  case  ((h z12) s12)  of {
                    Ctx s6 -> (Ctx (C1 s6 v15));
                    Val s ->
                      case  v15  of {
                        Lit u11 -> (Ctx (Hole (Sum s u11)));
                        Add r2 s9 ->
                          case  ((h r2) s9)  of { Ctx r12 -> (Ctx (C2 s r12)); Val u5 -> (Ctx (Hole (Sum s u5))); };
                      };
                  };
              }))
        in
          ((h x) z1))
         of {
          Ctx v13 ->
            (g
              (letrec
                f1=(\w15->
                  case  w15  of {
                    Hole r10 -> case  r10  of { Sum u r6 -> ((f u) r6); };
                    C1 r11 y15 -> (Add (f1 r11) y15);
                    C2 s14 z11 -> (Add (Lit s14) (f1 z11));
                  })
              in
                (f1 v13)));
          Val r14 -> r14;
        };
    })
in
  (g t))
