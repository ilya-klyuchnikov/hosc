data Number = Z | S Number;
data Boolean = True | False;

f n Z

where  
  
f = \r1 s1 ->
      case  r1  of {
        Z ->
          case  s1  of {
            S u ->
              case  u  of {
                S x -> (g x);
                Z -> False;
              };
            Z -> True;
          };
        S t -> ((f t) (S (S s1)));
      };
  
g = \t1 ->
case  t1  of { Z -> True; S r -> case  r  of { Z -> False; S v1 -> (g v1); }; };