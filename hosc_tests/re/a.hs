case  w  of {
  Nil  -> Fail;
  Cons y22 w8 ->
    case  y22  of {
      A  ->
        case  w8  of {
          Nil  -> (Parsed Nil);
          Cons z1 t17 ->
            case  z1  of {
              A  -> Fail;
              B  ->
                case  t17  of {
                  Nil  -> Fail;
                  Cons r7 x22 ->
                    case  r7  of {
                      A  ->
                        (letrec
                          f=(\u35->
                            case  u35  of {
                              Nil  -> (Parsed Nil);
                              Cons v33 t15 ->
                                case  v33  of {
                                  A  -> Fail;
                                  B  ->
                                    case  t15  of {
                                      Nil  -> Fail;
                                      Cons t31 s17 -> case  t31  of { A  -> (f s17); B  -> Fail; };
                                    };
                                };
                            })
                        in
                          (f x22));
                      B  -> Fail;
                    };
                };
            };
        };
      B  -> Fail;
    };
}