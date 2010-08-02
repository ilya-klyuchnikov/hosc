{-# LANGUAGE NoImplicitPrelude#-}

--dRec1  (dRec2 (dRec2 id id) (dRec1 id id))   (dRec1 (dRec1 id id) (dRec2 id id))

--sf id id

--(sf id) (id id)

--((sf id) (id (id id)))

--sf sf sf 

--((sf sf) (sf sf))
--((sf sf) (sf (sf sf)))

--((sf sf) (sf (sf (sf sf))))

--where

--sf = \x y -> sf x (y x);
--sf  = \x y -> sf (y x) y ;
--sf1 = \x y -> sf1 x (x y);

--sf2 = \x y -> sf2 x (y x); 

--((sf id) (id id))

--sf3 id id 

((sf id) (id id))

where

sf = \x y -> sf x (x y);

--sf3 = \x y -> sf3 x (y id);

id = \x -> x;

dRec1 = \x y -> dRec1 (dRec2 x y) (dRec1 y x);

dRec2 = \x y -> dRec2 (dRec1 x y) (dRec2 y x);

dRec3 = \x y -> dRec3 (dRec3 x y) (dRec3 y x);

rec1 = \x -> rec1 (rec2 x);

rec2 = \x -> rec2 (rec3 x);

rec3 = \x -> rec3 (rec1 x);

i = \x -> x; 
k = \x y -> x;  
s = \x y z -> x z (y z);

fix = \f -> f (fix f);

g = \f x y -> f x y y;

-- rec1 = \x -> rec1 (rec1 x);

-- rec2 = \x -> rec2 (rec1 x);