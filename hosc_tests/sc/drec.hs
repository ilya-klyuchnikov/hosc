dRec1 x x  where

dRec1 = \x y -> dRec1 (dRec2 x y) (dRec1 y x);

dRec2 = \x y -> dRec2 (dRec1 x y) (dRec2 y x);

rec1 = \x -> rec1 (rec2 x);

rec2 = \x -> rec2 (rec3 x);

rec3 = \x -> rec3 (rec1 x);

-- rec1 = \x -> rec1 (rec1 x);

-- rec2 = \x -> rec2 (rec1 x);