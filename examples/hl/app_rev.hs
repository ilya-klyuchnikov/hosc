data List a = Nil | Cons a (List a);

--app (rev xs) ys
appRev u v
where

app = \xs ys ->
    case xs of {
      Nil -> ys;
      Cons z zs -> Cons z (app zs ys);
    };

rev = \xs ->
   case xs of {
     Nil -> Nil;
     Cons y ys -> app (rev ys) (Cons y Nil);
   };

appRev = \xs ys -> app (rev xs) ys;