data Action  = RM  | WH2 ;
data Number  = Z  | S Number;
data State  = State Number Number Number | Stop ;
data Boolean  = True  | False ;
data List a = Nil  | Cons a (List a);

((f (State (S x) Z Z)) y)
where

f = (\v7->
  (\w7->
    case  w7  of {
      Nil  ->
        case  v7  of {
          State z4 x6 z6 ->
            case  x6  of {
              S p4 -> case  p4  of { S u4 -> False; Z  -> case  z6  of { S y4 -> False; Z  -> True; }; };
              Z  -> True;
            };
          Stop  -> True;
        };
      Cons p t ->
        ((f
            case  p  of {
              RM  ->
                case  v7  of {
                  State r v v4 -> case  r  of { Z  -> Stop; S w3 -> (State ((g w3) v) Z (S v4)); };
                  Stop  -> Stop;
                };
              WH2  ->
                case  v7  of {
                  State y5 p3 u7 ->
                    case  y5  of {
                      Z  -> case  u7  of { Z  -> Stop; S r5 -> (State ((h r5) p3) (S Z) Z); };
                      S t3 ->
                        case  u7  of {
                          Z  -> (State (S ((f1 p3) t3)) (S Z) Z);
                          S p6 -> (State (S (((g1 p6) p3) t3)) (S Z) Z);
                        };
                    };
                  Stop  -> Stop;
                };
            })
          t);
    }));
g1 = (\z8->
  (\u8->
    (\v8->
      case  z8  of {
        Z  -> ((h1 u8) v8);
        S u5 -> (S case  u8  of { Z  -> ((f2 u5) v8); S w2 -> (S (((g1 u5) v8) w2)); });
      })));
f2 = (\r8->
  (\s8->
    case  s8  of { Z  -> r8; S v6 -> (S ((f2 v6) r8)); }));
h1 = (\w8->
  (\p8->
    case  w8  of { Z  -> p8; S w4 -> (S ((h1 p8) w4)); }));
f1 = (\x8->
  (\y8->
    case  x8  of { Z  -> y8; S u6 -> (S ((f1 y8) u6)); }));
h = (\s7->
  (\t7->
    case  t7  of { Z  -> s7; S s5 -> (S ((h s5) s7)); }));
g = (\p7->
  (\r7->
    case  p7  of { Z  -> r7; S v3 -> (S ((g r7) v3)); }));