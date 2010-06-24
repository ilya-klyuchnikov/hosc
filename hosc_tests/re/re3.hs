data Alphabet  = A  | B ;
data List a = Nil  | Cons a (List a);
data Bool  = True  | False ;
data Option a = None  | Some a;

f ws

--case ((h v14)) of {None  -> (f (Cons (A) v14)); Some v18 -> (Some v18);}

where

f = \t47->
  case  t47  of {
    Nil  -> Some Nil;
    Cons y40 t29 -> 
    	case  y40  of { 
    		A  -> case  (h t29)  of {None  -> (f t29); Some z18 -> (Some z18); }; 
    		B  -> None; 
    	};
  };
----
h = \y48->
  case  y48  of { 
  	Nil  -> Some Nil; 
  	Cons s33 y41 -> 
  		case  s33  of {
  			A  -> h y41; 
  			B  -> None; 
  		}; 
  };

{-
(letrec
  f=(\t47->
    case  t47  of {
        Nil  -> (Some Nil);
        Cons y40 t29 ->
          case  y40  of {
            A  ->
                case 
                (letrec
                  h=(\y48->
                    case  y48  of {
                        Nil  -> (Some Nil);
                        Cons s33 y41 -> case  s33  of { A  -> (h y41); B  -> None; };
                      })
                in
                  (h t29))
                 of {
                  None  -> (f t29);
                  --Some z18 -> choice{(Some (Cons A t29)); (f z18);};
                  Some z18 -> (f z18);
                };
            B  -> None;
          };
      })
in
  (f w))
  
-}
