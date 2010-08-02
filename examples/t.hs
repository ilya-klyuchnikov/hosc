
	eval (App (Var (VZ )) (Var (VZ ))) (Bind VZ (eval ( Lam VZ (App (Var VZ) (Var VZ)) ) e) e)


(
	(eval (App (Var (VZ )) (Var (VZ )))) 
	(Bind VZ 
		(eval (Var VZ) (Bind VZ ((eval (Var VZ)) (Bind (VZ ) 
			(eval ( Lam VZ (App (Var VZ) (Var VZ)) ) e) 
		e)) e)) 
	e)
)


was:
(eval ( Lam VZ (App (Var VZ) (Var VZ)) ) e) 
became:
(eval (Var VZ) (Bind VZ ((eval (Var VZ)) (Bind (VZ ) (eval ( Lam VZ (App (Var VZ) (Var VZ)) ) e) e)) e))

((eval (Lam (VZ ) (App (Var (VZ )) (Var (VZ ))))) e)

(((((eval (NatS (Var (VZ )))) (Bind (VZ ) (((evalFix (VZ )) (NatS (Var (VZ )))) (Empty )) (Empty ))) (\$280 -> (Error )))

(\$281 -> (N $281)) ) (\$282 -> (\$283 -> (\$284 -> (C $282 $283 $284)))) )



(((((eval (NatS (Var (VZ )))) (Bind (VZ ) (((evalFix (VZ )) (NatS (Var (VZ )))) (Empty )) (Empty ))) (\$1043 -> (Error ))) 
(\$1044 -> ((\$1045 -> (N $1045)) (S $1044))) ) (\$1046 -> (\$1047 -> (\$1048 -> ((\$1049 -> (Error )) (U ))))) )