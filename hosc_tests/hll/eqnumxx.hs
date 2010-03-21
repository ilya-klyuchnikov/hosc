data Number = Z | S Number;
data Boolean = True | False;
data Pair = P Number Number;

eqnum (add x x) (add x x)

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