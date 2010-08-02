{-# LANGUAGE NoImplicitPrelude#-}

data Unit = U;
data Bool = True | False;
data Nat = Z | S Nat;

data VarName = VZ | VS VarName;

data Exp = NatZ | NatS Exp | Var VarName | App Exp Exp | Lam VarName Exp | Fix VarName Exp;

data Env
  = Empty
  | Bind VarName
    ((Unit -> Val) -> (Nat -> Val) -> (VarName -> Exp -> Env -> Val) -> Val) Env;

data Val = Error | N Nat | C VarName Exp Env;

-- eval where

-- eval (Fix VZ (NatS (Var VZ))) Empty (\u -> Error) (\n -> N n) (\v1 e1 env1 -> C v1 e1 env1) where

-- e where

run (Fix VZ (NatS (Var VZ)))
--run g (Fix VZ (NatS (Var VZ)))
--run (App (Lam VZ (NatS (Var VZ))) NatZ)

where

varNameEq = \x y ->
  case x of {
    VZ -> case y of {VZ -> True; VS y1 -> False;};
    VS x1 -> case y of {VZ -> False; VS y1 -> varNameEq x1 y1;};
  };

lookup = \v env ->
  case env of {
    Empty -> (\ke kn kc -> ke U);
    Bind w val env1 ->
      case (varNameEq v w) of {
        True -> val;
        False -> lookup v env1;
    };
  };

eval = \e env ke kn kc ->
  case e of {
    NatZ -> kn Z;
    NatS e1 -> (eval e1 env) ke (\n1 -> kn (S n1)) (\v0 e0 env0 -> ke U);
    Var v -> lookup v env ke kn kc;
    Lam v body -> kc v body env;
    App e1 e2 ->
      (eval e1 env) ke (\n -> ke U)
        (\v body env1 -> eval body (Bind v (eval e2 env) env1) ke kn kc);
    Fix v body -> evalFix v body env ke kn kc;
    -- Fix v body -> eval body (Bind v (evalFix v body env) env) ke kn kc;
    -- Fix v body -> eval body (Bind v (eval (Fix v body) env) env) ke kn kc;
  };

evalFix = \v body env ke kn kc-> eval body (Bind v (evalFix v body env) env) ke kn kc;
--evalFix = \v body env ke kn kc-> eval body (Bind v (eval (Fix v body) env) env) ke kn kc;
-- evalFix = \v body env ke kn kc -> eval (App (Lam v body) (Fix v body)) ke kn kc;

run = \e -> eval e Empty (\u -> Error) (\n -> N n) (\v1 e1 env1 -> C v1 e1 env1);
--run = \f -> \e -> eval e Empty (\u -> Error) f (\v1 e1 env1 -> Error);
--run = \e -> eval e Empty (\u -> Error) (\n -> N n) (\v1 e1 env1 -> Error);