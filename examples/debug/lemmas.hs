--decCtx CtxHole c => Val c;
--decCtx (CtxLeft f ctx0 t2) c => decTerm t2 (CtxRight f c ctx0);
--decCtx (CtxRight f c1 ctx0) c => Dec (Redex f c1 c) ctx0;

--contract (Redex f c1 c2) => Cst (f c1 c2);

--recompose CtxHole t => t;
--recompose (CtxLeft f ctx0 t2) t => recompose ctx0 (Opr f t t2);
--recompose (CtxRight f c ctx0) t => recompose ctx0 (Opr f (Cst c) t);