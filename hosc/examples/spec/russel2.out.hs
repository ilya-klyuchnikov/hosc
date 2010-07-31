data Bool  = True | False ;
data U  = MkU (U -> Bool);

letrec f = f in f
