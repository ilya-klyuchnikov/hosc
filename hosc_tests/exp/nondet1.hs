data Pair a b = P a b;
data Option a = Some a | None;
data Enum = A | B;


choice {map1; map2;} choice{change1; id;} (P A B) where


change = \e -> case e of {A -> B; B -> A;};
id = \e -> e;

change1 = \x -> choice{change x; id x;};
 
map1 = \f p -> case p of {P e1 e2 -> P (f e1) e2;};
map2 = \f p -> case p of {P e1 e2 -> P (f e1) e2;};