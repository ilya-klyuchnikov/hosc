
(letrec
  f=(\u15->
    (\v15->
      case  v15  of {
        Cons v5 t5 ->
          case  v5  of {
            A  ->
              case  t5  of {
                Cons t6 r14 ->
                  case  t6  of {
                    A  ->
                      case  (u15 r14)  of {
                        Some z14 -> (Some z14);
                        None  ->
                          ((f (\y2-> case  y2  of { Cons z1 w11 -> case  z1  of { A  -> (u15 w11); }; Nil  -> None; }))
                            (Cons A r14));
                      };
                  };
                Nil  ->
                  ((f (\p10-> case  p10  of { Cons p8 t7 -> case  p8  of { A  -> (u15 t7); }; Nil  -> None; })) Nil);
              };
          };
        Nil  -> None;
      }))
in
  ((f (\p-> case  p  of { Cons p2 x14 -> None; Nil  -> (Some Nil); })) (Cons v62 v63)))
---
(letrec
  f=(\r22->
    (\s22->
      case  s22  of {
        Cons s12 r9 ->
          case  s12  of {
            A  ->
              case  r9  of {
                Cons p13 r2 ->
                  case  p13  of {
                    A  ->
                      case  (r22 r2)  of {
                        Some y4 -> (Some y4);
                        None  ->
                          ((f (\s19-> case  s19  of { Cons s2 z18 -> case  s2  of { A  -> (r22 z18); }; Nil  -> None; }))
                            (Cons A r2));
                      };
                  };
                Nil  ->
                  ((f (\u6-> case  u6  of { Cons p1 s14 -> case  p1  of { A  -> (r22 s14); }; Nil  -> None; })) Nil);
              };
          };
        Nil  -> None;
      }))
in
  ((f (\u8-> case  u8  of { Cons s18 w7 -> None; Nil  -> (Some Nil); })) (Cons v136 v137)))