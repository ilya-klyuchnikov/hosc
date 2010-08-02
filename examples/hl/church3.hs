case  x s (x s Z)  of {
  Z  -> case  (x s Z)  of { Z  -> True; S $173 -> False; };
  S $53 ->
    case  (x s Z)  of {
      Z  -> False;
      S $73 ->
        (letrec
          f=(\y->
            (\z->
              case  y  of {
                Z  -> case  z  of { Z  -> False; S $143 -> False; };
                S $92 ->
                  case  z  of { Z  -> case  $92  of { Z  -> True; S $129 -> False; }; S $106 -> ((f $92) $106); };
              }))
        in
          ((f $53) $73));
    };
}

case (x s Z) of {
	Z -> case x s (x s Z) of {Z -> True; S $173 -> False;};
	S $73 -> case x s (x s Z) of {
		Z -> False;
		S $53 -> (letrec
          f=(\y->
            (\z->
              case  y  of {
                Z  -> case  z  of { Z  -> False; S $143 -> False; };
                S $92 ->
                  case  z  of { Z  -> case  $92  of { Z  -> True; S $129 -> False; }; S $106 -> ((f $92) $106); };
              }))
        in
          ((f $53) $73));
	};
	 
}