{-# LANGUAGE NoImplicitPrelude#-}

data Tree a = Stub | Leaf a | Node (Tree a) (Tree a);
data TreeNF a = StubNF | NodeNF a (TreeNF a);
data Redex a = PRLeftStub (Tree a) | PRLeaf a | PRAssoc (Tree a) (Tree a) (Tree a);
data Contr a = Contr (Tree a) | Error;
data Ctx a = CtE | CtR a (Ctx a);

data Dec a = Val (Tree a) (TreeNF a) | Dec (Redex a) (Ctx a);

data Res a = Res (TreeNF a) | Bug;

--flatten tree where

--normalize tree

--decompose (recompose c t)
--decomposeTree t c

--decomposeTree (recompose c1 t) CtE

decomposeTree t c1

where

normalize = \t -> iterate (decompose t);

--iterate :: Dec a -> Res a;
iterate = \dec  -> case dec of {
	Val t1 t2 -> Res t2;
	Dec pr c -> 
		case (contract pr) of {
			Contr t -> iterate (decompose (recompose c t)); 
			Error -> Bug;
		}; 
};

decompose = \tree -> decomposeTree tree CtE;

recompose = \ctx tree -> case ctx of {
	CtE -> tree;
	CtR n1 c -> recompose c (Node (Leaf n1) tree);
};

contract = \redex -> case redex of {
	PRLeftStub t -> Contr t;
	PRLeaf n -> Contr (Node (Leaf n) Stub);
	PRAssoc t11 t12 t2 -> Contr (Node t11 (Node t12 t2));
};


decomposeTree = \tree c -> 
	case tree of {
		Stub -> decomposeCtx c  Stub StubNF;
		Leaf n -> Dec (PRLeaf n) c;
		Node t1 t2 -> decomposeNode t1 t2 c; 
	};
	
decomposeNode = \t1 t2 c ->
	case t1 of {
		Stub -> Dec (PRLeftStub t2) c;
		Leaf n -> decomposeTree t2 (CtR n c);
		Node t11 t12 -> Dec (PRAssoc t11 t12 t2) c;
	};
	
decomposeCtx = \ctx t tnf -> 
	case ctx of {
		CtE -> Val t tnf;
		CtR n c -> decomposeCtx c (Node (Leaf n) t) (NodeNF n tnf);
	};
	
flatten = \tree -> case tree of {
	Stub -> StubNF;
	Leaf n -> NodeNF n StubNF;
	Node t1 t2 -> flattenNode t1 t2; 
};

flattenNode = \t1 t2 -> case t2 of {
	Stub -> flatten t2;
	Leaf n -> NodeNF n (flatten t2);
	Node t11 t12 -> flattenNode t11 (Node t12 t2);
};