
// State x_monkey on_floor? x_box goal?  x_ban
data State = State Num Boolean Num Boolean Num;
data Num = Z | S Num;
data Boolean = True | False;

data Action = GoTo Num | MoveBoxTo Num | Climb | Grasp;


do = \action state eq ->
	case state of {
		State xm fl xb g -> 
			case action of {
				GoTo x -> State x fl xbox g xban;
				MoveBoxTo x -> case eq xm xb of {False -> state; True -> State x fl x g xban;}
				Climb -> case eq xm xb of {False -> state; True -> State x fl x g;}
			};
	};





