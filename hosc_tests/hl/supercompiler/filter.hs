data List a = Nil | Cons a (List a);
data Boolean = True | False;

map f (filter (compose p f) xs)


where

compose = \f g x ->  f (g x);

map = \f xs ->
  case xs of {
    Nil -> Nil;
    Cons x1 xs1 -> Cons (f x1) (map f xs1);
  };

filter = \p xs ->
  case xs of {
    Nil -> Nil;
    Cons x xs1 ->
      case p x of {
         True -> Cons x (filter p xs1);
         False -> filter p xs1;
      };
  };