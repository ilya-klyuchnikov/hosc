data Number = Z | S Number;
data Boolean = True | False;
data Pair = P Number Number;

((eqnum ((add x) (S y))) ((add y) (S x)))

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

add2 = \p ->
  case p of {
    P x y ->
      case x of {
        Z -> y;
        S x1-> add2 (P x1 (S y));
      };
  };

eqnum2 = \p ->
  case p of {
    P x y ->
      case x of {
        Z -> case y of {Z -> True; S y1 -> False;};
        S x1 -> case y of {Z-> False; S y1 -> eqnum2 (P x1 y1);};
      };
  };