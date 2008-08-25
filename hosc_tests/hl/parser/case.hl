number :: Z | S number;
boolean :: True | False;

even (plus x x)

where

even = %x {
  case x of {
    Z : True;
    S y : case y of {
       Z : False;
       S z : even z
    };
  }
}

double = %x { %y {
  case x of {
    Z : y;
    S z : double z (S (S y));
  }
}}

plus = %x{ %y {
case x of {Z: y; S x1: S (plus x1 y);}
}}