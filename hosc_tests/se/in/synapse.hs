data Action = RM | WH2;
data Number = Z | S Number;
data State = State Number Number Number | Stop;
data Boolean = True | False;
data List a = Nil | Cons a (List a);

loop (State (S x) Z Z) y

where

loop = \state acts ->
    case acts of {
      Nil -> test state;
      Cons a as -> loop (act a state) as;
    };

add = \x y ->
    case x of {
      Z   -> y;
      S x1 -> S (add x1 y);
    };

act = \a state ->
    case a of {
      RM -> case state of {
        State i d v -> case i of {Z -> Stop; S x-> State (add x d) Z (S v);};
        Stop -> Stop;
      };
      WH2 -> case state of {
        State i d v -> case i of {
                        Z -> case v of {Z-> Stop; S x-> State (add (add i d) x) (S Z) Z;};
                        S x -> case v of {Z-> State (add (add i d) v) (S Z) Z; S y-> State (add (add i d) y) (S Z) Z;};
                      };
        Stop -> Stop;
      };
    };

test = \state ->
  case state of {
    State i d v -> case d of {
                    S d1 -> case d1 of {S d2-> False; Z -> case v of {S x -> False; Z-> True;};};
                    Z -> True;
                  };
    Stop -> True;
  };