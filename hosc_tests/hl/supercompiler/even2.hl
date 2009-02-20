data Number = Z | S Number;
data Boolean = True | False;     
      
 (\w1 p1 ->
          case  w1  of {
            Z ->  (g p1);
            S y1 -> ((f y1) (S (S p1)));
          }
      ) t Z
      
where
    
g = \t1 ->
                  case  t1  of { Z -> True; S z -> case  z  of { Z -> False; S r -> (g r); }; };
                
f = \w1 p1 ->
          case  w1  of {
            Z ->  (g p1);
            S y1 -> ((f y1) (S (S p1)));
          };