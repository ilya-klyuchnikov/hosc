data Alphabet  = A  | B ;
data List a = Nil  | Cons a (List a);
data Bool  = True  | False ;
data Option a = None  | Some a;

case (h x) of {None  -> f (Cons A x); Some y -> Some y;}

where

f = \w->
  case  w  of {
    Nil  -> Some Nil;
    Cons l w1 -> 
    	case  l  of { 
    		A  -> case  (h w1)  of {None  -> (f w1); Some w2 -> (Some w2); }; 
    		B  -> None; 
    	};
  };

h = \w->
  case  w  of { 
  	Nil  -> Some Nil; 
  	Cons l w1 -> 
  		case  l  of {
  			A  -> h w1; 
  			B  -> None; 
  		}; 
  };