data Unit = U;
data Pair a b = P a b;
data Bool = True | False;
data Nat = Z | S Nat;

--run (app (lam (app varZ varZ)) (lam (app varZ varZ)))

run

where

fst = \v -> case v of { P x y -> x;};
snd = \v -> case v of { P x y -> y;};

varZ = \env -> fst env;
varS = \v env -> v (snd env);

cst = \a env -> a;

natZ = \env -> Z;
natS = \n env -> S(n env);

lam = \e env x -> e (P x env);
app = \e1 e2 env -> (e1 env) (e2 env);
fix = \f env -> f (P (fix f env) env);
if = \e0 e1 e2 env ->
  case e0 env of {
    True -> e1 env;
    False -> e2 env;
  };

run = \e -> e U;