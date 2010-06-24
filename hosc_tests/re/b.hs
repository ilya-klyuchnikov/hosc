case  w  of {
  Nil  -> Fail;
  Cons y7 u46 ->
    case  y7  of {
      A  ->
        case  u46  of {
          Nil  -> (Parsed Nil);
          Cons r40 x40 ->
            case  r40  of {
              A  -> Fail;
              B  ->
                (letrec
                  f=(\v46->
                    case  v46  of {
                      Nil  -> Fail;
                      Cons p32 x19 ->
                        case  p32  of {
                          A  ->
                            case  x19  of {
                              Nil  -> (Parsed Nil);
                              Cons x30 w17 -> case  x30  of { A  -> Fail; B  -> (f w17); };
                            };
                          B  -> Fail;
                        };
                    })
                in
                  (f x40));
            };
        };
      B  -> Fail;
    };
}