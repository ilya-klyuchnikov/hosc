data Number = Z | S Number;
data Boolean = True | False;

--eqnum (add (S x) (S y)) (add (S y) (S x))
eqnum (add x y) (add y x)

--eqnum (add x (S y)) (add y (S x))

where

add = \x y ->
    case x of {
      Z   -> y;
      S n -> S (add n y);
    };

eqnum = \x y ->
    case x of {
      Z -> case y of { Z -> True; S z -> False;};
      S n -> case y of { Z -> False; S z -> eqnum n z; };
    };