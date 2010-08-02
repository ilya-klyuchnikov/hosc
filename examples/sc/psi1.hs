data List a = Nil | Cons a (List a);
data Enum = A | B;
data B = True | False;

contains x (app xs (app (Cons x Nil) zs))
where

app = \xs ys ->
    case xs of {
      Nil -> ys;
      Cons z zs -> Cons z (app zs ys);
    };

contains = \x xs ->
  case xs of {
    Nil -> False;
    Cons x1 xs1 -> or (eq x1 x) (contains x xs1);
  };

eq = \x y -> case x of {
  A -> case y of {A -> True; B -> False;};
  B -> case y of {A -> False; B -> True;};
};

or = \x y -> case x of {True -> True;False -> y;};