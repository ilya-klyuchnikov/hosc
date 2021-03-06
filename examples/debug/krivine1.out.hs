-- generated by hosc0 from debug/krivine1.hs

data Nat  = Z  | S Nat;
data List a = Nil  | Cons a (List a);
data Term  = Var Nat | Lam Term | App Term Term | Ccc Term;
data Closure  = CloGnd Term (List Closure) | CloApp Closure Closure | CloCcc Closure | CloCtx Context;
data Context  = RC0  | RC1 Context Closure | RC2 Context;
data Redex  = Access Nat (List Closure) | Beta Closure | PropApp Term Term (List Closure) | PropCcc Term (List Closure) | Cwcc Closure;
data Option a b = None  | Some a b;

case  r  of {
  Access z3 t1 ->
    (Some
      (letrec f=(\w3-> (\p3-> case  w3  of { Cons v3 u3 -> case  p3  of { Z  -> v3; S v1 -> ((f u3) v1); }; }))
      in
        ((f t1) z3))
      rc);
}
