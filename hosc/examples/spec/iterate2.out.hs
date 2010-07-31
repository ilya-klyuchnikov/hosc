data List a = Nil | Cons a (List a);  

letrec g= \v-> Cons (f v) (g (f v)) 
in g x 