data List a = Nil | Cons a (List a);
data Bool = True | False;

(letrec g=(Cons True g) in g)