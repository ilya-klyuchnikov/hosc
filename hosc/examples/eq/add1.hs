data Number = Z | S Number;

add x (add y z) where

add = \x y ->
    case x of {
      Z -> y;
      S x1 -> S (add x1 y);
    };