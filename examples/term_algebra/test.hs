// all binders are different here
data List a = Nil | Cons a (List a);
data Boolean = True | False;
data Number = Z | S Number;

zzzz where

f = \x -> x;
g = \y -> 
  case y of {
    True -> True;
    False -> f y;
  };

app = \xs ys ->
    case xs of {
      Nil -> ys;
      Cons z zs -> Cons z (app zs xs);
    };

rev = \l -> 
  case l of {
    Nil -> Nil;
    Cons e l1  -> app (rev l1) (Cons e Nil);
  };

f1 = \x1 y1 -> 
    case x1 of {
      Nil -> y1;
      Cons z1 zs1 -> Cons z1 (app zs1 x1);
    };

f2 = \x2 y2 -> 
    case x2 of {
      Nil -> y2;
      Cons z2 zs2 -> Cons z2 (app zs2 x2);
    };

f3 = \x3 y3 ->
    case x3 of {
      Nil -> y3;
      Cons z3 zs3 -> Cons z3 zs3;
    };

f4 = \xs4 -> rev xs4;
f5 = \xs5 -> app (rev xs5) Nil;

f6 = \x6 y6 -> 
    case (rev x6) of {
      Nil -> y6;
      Cons z6 zs6 -> Cons z6 (app zs6 x6);
    };

f7 = \x7 y7 ->
    case rev x7 of {
      Nil -> y7;
      Cons z7 zs7 -> Cons z7 (app zs7 x7);
    };

f8 = \x8 y8 ->
    case 
      case rev x8 of {
        Nil -> y8;
        Cons z9 zs9 -> Cons z9 (app zs9 x8);
      } 
    of {
      Nil -> y8;
      Cons z8 zs8 -> Cons z8 (app zs8 x8);
    };
    
plus = \x2 y2 ->
  		case x2 of {
  			Z -> y2; 
  			S x1 -> S (plus x1 y2);
  		};

eqnum = \x3 y3 ->
	case x3 of {
		Z -> case y3 of {Z -> True; S y4 -> False;};
		S x5 -> case y3 of {Z -> False; S y5 -> eqnum x5 y5;};
	};
	
g1 = \x -> x;
g2 = \x -> S x;

