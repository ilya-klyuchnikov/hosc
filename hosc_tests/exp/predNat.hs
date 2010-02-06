data Unit = U;
data Bool = True | False;

predNat (natS natZ)

where

var = \x -> x;
lam = \f -> f;
app = \e1 e2 -> e1 e2;

cst = \a -> a;

fix = \f -> f (fix f);

if = \e0 e1 e2 ->
  case e0 of {
    True -> e1;
    False -> e2;
  };

natZ = \z s -> z;
natS = \n z s -> s n;
caseNat = \n z s -> n z s;
predNat = \e -> caseNat e (case U of {})(\n1 -> n1);

app2 = \e1 e2 e3 -> app (app e1 e2) e3;