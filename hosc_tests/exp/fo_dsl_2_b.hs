-- using native error propagation

data Nat = Z | S Nat;
data Val = Error | N Nat | F (Val -> Val);

data VarName = VZ | VS VarName;

data Exp =
  NatZ |
  NatS Exp |
  Var VarName | App Exp Exp | Lam VarName Exp |
  Fix VarName Exp;
  
data Env = Empty | Bind VarName Val Env;
data Bool = True | False;

--run (Fix VZ (App (Var VZ) (Var VZ)))
run (App (Lam VZ (App (Var VZ) (Var VZ))) (Lam VZ (App (Var VZ) (Var VZ))))

where

varNameEq = \x y ->
  case x of {
    VZ -> case y of {VZ -> True; VS y1 -> False;};
    VS x1 -> case y of {VZ -> False; VS y1 -> varNameEq x1 y1;};
  };

lookup = \v env ->
  case env of {
    Bind w val env1 ->
      case (varNameEq v w) of {
        True -> val;
        False -> lookup v env1;
    };
  };

run = \e -> eval e Empty;

eval = \e env ->
  case e of {
    NatZ -> N Z;
    NatS e1 -> evalNatS (eval e1 env);
    Var v -> lookup v env;
    Lam v body -> 
      F (\x -> eval body (Bind v x env));
    App e1 e2 ->
      case eval e1 env of {
        F f -> f (eval e2 env);
      };
    Fix v body -> evalFix v body env;
  };

evalNatS = \x ->
  case x of {
    N n -> N (S n);
  };

evalFix = \v body env ->
  eval body (Bind v (evalFix v body env) env);