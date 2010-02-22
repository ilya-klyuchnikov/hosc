data Bool = True | False;
data List a = Nil | Cons a (List a);
data Tree a = Leaf a | Fork (Tree a) (Tree a);

--eqBoolList Nil Nil
--eqBoolList (Cons True Nil) Nil
--eqBoolList Nil (Cons True Nil)
--eqBoolList (Cons True Nil) (Cons True Nil)
--eqBoolList (Cons True Nil) (Cons False Nil)
eqTree p q
--eqTreeAcc p q

where

flatten = \t -> flattenAcc t Nil;

flattenAcc = \t u -> case t of {
  Leaf a -> Cons a u;
  Fork t1 t2 -> flattenAcc t1 (flattenAcc t2 u);
  };

not = \x -> case x of {
  True -> False;
  False -> True;
  };

eqBool = \x y -> case x of {
  True -> y;
  False -> not y;
  };

eqList = \eq u v -> case u of {
  Nil -> case v of {
    Nil -> True;
    Cons y v1 -> False;
    };
  Cons x u1 -> case v of {
    Nil -> False;
    Cons y v1 -> case eq x y of {True -> eqList eq u1 v1; False -> False;};
    };
  };

and = \ x y -> case x of {
  True -> y;
  False -> False;
};

eqTree = \u v -> case u of {
  Leaf a -> case v of { Leaf b -> eqBool a b; Fork u1 u2 -> False; };
  Fork u1 u2 -> case v of { Leaf b -> False; Fork v1 v2 -> and (eqTree u1 v1) (eqTree u2 v2); };
};

eqTreeAcc = \u v -> eqList eqTree (Cons u Nil) (Cons v Nil);