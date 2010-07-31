data Number = Z | S Number;
data Boolean = True | False;

f x (S Z) d
where

f = \p r h ->
    case  p  of { Z  -> h r; S w -> f w (S r) h; };