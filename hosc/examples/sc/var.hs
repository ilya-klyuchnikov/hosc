data Num = Z | S Num;

h a b

where

h = \x y ->
  case x of {
     Z -> y;
     S a -> h a y;
  };