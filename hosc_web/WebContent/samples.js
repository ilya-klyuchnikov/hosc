var programs = new Array();

programs["app x y"]=
"list $a :: Nil | Cons $a (list $a);\n\
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

var sample = function(sampleName) {
  var expr = document.getElementById('expression');
  expr.value = sampleName;
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