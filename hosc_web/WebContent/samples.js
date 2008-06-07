var programs = new Array();

programs["app x y"]=
"list $a :: Nil | Cons $a (list $a);\n\
\n\
app x y\n\
where\n\
\n\
app = %xs {\n\
  %ys {\n\
    case xs of {\n\
      Nil : ys;\n\
      Cons z zs : Cons z (app zs ys);\n\
    }\n\
  }\n\
}"

programs["app (app x y) z"]=
"list $a :: Nil | Cons $a (list $a);\n\
\n\
app (app x y) z\n\
where\n\
\n\
app = %xs {\n\
  %ys {\n\
    case xs of {\n\
      Nil : ys;\n\
      Cons z zs : Cons z (app zs ys);\n\
    }\n\
  }\n\
}"

programs["app (app x y) x"]=
"list $a :: Nil | Cons $a (list $a);\n\
\n\
app (app x y) x\n\
where\n\
\n\
app = %xs {\n\
  %ys {\n\
    case xs of {\n\
      Nil : ys;\n\
      Cons z zs : Cons z (app zs ys);\n\
    }\n\
  }\n\
}"

programs["take n m"]=
"list $a :: Nil | Cons $a (list $a);\n\
number :: Z | S number;\n\
\n\
take n m\n\
where\n\
\n\
from = %n {\n\
  Cons n (from (S n))\n\
}\n\
\n\
take = %n {\n\
  %xs {\n\
    case n of {\n\
      Z : Nil;\n\
      S x : case xs of {\n\
              Nil : Nil;\n\
              Cons h t : Cons h (take x t);\n\
            };\n\
    }\n\
  }\n\
}"

programs["map add1 (from x)"]=
"list $a :: Nil | Cons $a (list $a);\n\
number :: Z | S number;\n\
\n\
map add1 (from x)\n\
where\n\
\n\
from = %n {\n\
  Cons n (from (S n))\n\
}\n\
\n\
map = %f {\n\
  %list {\n\
    case list of {\n\
      Nil : Nil;\n\
      Cons x xs : Cons (f x) (map f xs);\n\
    }\n\
  } \n\
}\n\
\n\
add1 = %x {S x}\n\
sub1 = %x {\n\
  case x of {\n\
    Z : Z;\n\
    S y : y;\n\
  }\n\
}"

programs["map sub1 (from x)"]=
"list $a :: Nil | Cons $a (list $a);\n\
number :: Z | S number;\n\
\n\
map sub1 (from x)\n\
where\n\
\n\
from = %n {\n\
  Cons n (from (S n))\n\
}\n\
\n\
map = %f {\n\
  %list {\n\
    case list of {\n\
      Nil : Nil;\n\
      Cons x xs : Cons (f x) (map f xs);\n\
    }\n\
  } \n\
}\n\
\n\
add1 = %x {S x}\n\
sub1 = %x {\n\
  case x of {\n\
    Z : Z;\n\
    S y : y;\n\
  }\n\
}"

programs["map sub1 (map add1 (from x))"]=
"list $a :: Nil | Cons $a (list $a);\n\
number :: Z | S number;\n\
\n\
map sub1 (map add1 (from x))\n\
where\n\
\n\
from = %n {\n\
  Cons n (from (S n))\n\
}\n\
\n\
map = %f {\n\
  %list {\n\
    case list of {\n\
      Nil : Nil;\n\
      Cons x xs : Cons (f x) (map f xs);\n\
    }\n\
  } \n\
}\n\
\n\
add1 = %x {S x}\n\
sub1 = %x {\n\
  case x of {\n\
    Z : Z;\n\
    S y : y;\n\
  }\n\
}"

programs["even (double n Z)"]=
"number :: Z | S number;\n\
boolean :: True | False;\n\
\n\
even (double n Z)\n\
\n\
\n\
where\n\
\n\
even = %x {\n\
  case x of {\n\
    Z : True;\n\
    S y : case y of {\n\
       Z : False;\n\
       S z : even z;\n\
    };\n\
  }\n\
}\n\
\n\
double = %x { %y {\n\
  case x of {\n\
    Z : y;\n\
    S z : double z (S (S y));\n\
  }\n\
}}"

programs["rev x"] = 
"list $a :: Nil | Cons $a (list $a);\n\
boolean :: True | False;\n\
\n\
rev x\n\
\n\
where\n\
\n\
rev = %xs {\n\
  case xs of {\n\
    Nil : Nil;\n\
    Cons z  zs : app (rev zs) (Cons z Nil);\n\
  }\n\
}\n\
\n\
app = %xs {\n\
  %ys {\n\
    case xs of {\n\
      Nil : ys;\n\
      Cons z zs : Cons z (app zs ys);\n\
    }\n\
  }\n\
}"
programs["even1"] = 
"number  :: Z  | S number;\n\
boolean  :: True  | False ;\n\
\n\
f n Z\n\
\n\
where\n\
\n\
f = %r1 {\n\
    %s1 {\n\
      case  r1  of {\n\
        Z :\n\
          case  s1  of {\n\
            S u :\n\
              case  u  of {\n\
                S x : (g x);\n\
                Z : False;\n\
              };\n\
            Z : True;\n\
          };\n\
        S t : ((f t) (S (S s1)));\n\
      }\n\
    }\n\
  }\n\
\n\
g = %t1 {case  t1  of { Z : True; S r : case  r  of { Z : False; S v1 : (g v1); }; }}"

programs["leq (length (map even x)) (length x)"] = 
"list $a :: Nil | Cons $a (list $a);\n\
number :: Z | S number;\n\
boolean :: True | False;\n\
\n\
leq (length (map even x)) (length x)\n\
\n\
where\n\
\n\
length = %l {\n\
  case l of {Nil : Z; Cons x1 xs: S (length xs);}\n\
}\n\
\n\
even = %x {\n\
  case x of {\n\
    Z : True;\n\
    S y : case y of {\n\
       Z : False;\n\
       S z : even z;\n\
    };\n\
  }\n\
}\n\
\n\
map = %f {\n\
  %list {\n\
    case list of {\n\
      Nil : Nil;\n\
      Cons x xs : Cons (f x) (map f xs);\n\
    }\n\
  }\n\
}\n\
\n\
leq = %x {%y{\n\
  case x of {\n\
    Z: True;\n\
    S x1: case y of {Z: False; S y1: leq x1 y1;};\n\
  }\n\
\n\
}}"

programs["even (plus x x)"] = 
"number :: Z | S number;\n\
boolean :: True | False;\n\
\n\
even (plus x x)\n\
\n\
where\n\
\n\
even = %x {\n\
  case x of {\n\
    Z : True;\n\
    S y : case y of {\n\
       Z : False;\n\
       S z : even z;\n\
    };\n\
  }\n\
}\n\
\n\
double = %x { %y {\n\
  case x of {\n\
    Z : y;\n\
    S z : double z (S (S y));\n\
  }\n\
}}\n\
\n\
plus = %x{ %y {\n\
case x of {Z: y; S x1: S (plus x1 y);}\n\
}}"

var sample = function(sampleName) {
  var programText = document.getElementById('programText');
  programText.value = programs[sampleName];
  return false;	
}

var samplesHTML = function () {
  var s = "";
  for (v in programs) {
    s = s + "<a href='javascript:sample(\"" + v + "\");void(0);'>" + v + "</a> ";
  }
  return s;
}