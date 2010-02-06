data Nat = Z | S Nat;
data Bool = True | False;

--eq (f x) (f x) where
--eq1 m m where
--((eq1 (S (S (S (S m))))) (S (S (S (S m))))) 
--(letrec g=(\w1-> (g (S (S w1)))) in (g (S (S (letrec f=(S (S f)) in f)))))

eq1 x x
where

n = S n;
m = S (S m);

eq = \x y -> case x of {
  Z    -> case y of {Z -> True;  S y1 -> False;   } ;
  S x1 -> case y of {Z -> False; S y1 -> eq x1 y1;} ;
};

eq1 = \x y -> case x of {
  Z    -> case y of {Z -> True;  S y1 -> False;   } ;
  S x1 -> case y of {Z -> False; S y1 -> eq1 (S (S (S x1))) (S (S (S y1)));} ;
};

--f = \x -> S (S (f x));