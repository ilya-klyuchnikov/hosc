{-# LANGUAGE NoImplicitPrelude#-}

data List a = Nil | Cons a (List a);
data Boolean = True | False;

data RegExp s =
  Empty |
  Symb s |
  Seq (RegExp s) (RegExp s) |
  Alt (RegExp s) (RegExp s) |
  Rep (RegExp s);
  
regRepTrue i where

eqBoolean = \x y ->
  case x of {
    True -> case y of { True -> True; False -> False; };
    False -> case y of { False -> True; True-> False; };
  };

null = \xs ->
  case xs of {
    Nil -> True;
    Cons h t -> False;
};

reg = \e  input -> match eqBoolean e null input;

match = \eq r c input ->
  case r of {
    Empty ->
      c input;
    Symb s ->
      case input of {
        Nil -> False;
        Cons head tail ->
          case eq s head of {
            True -> c tail;
            False -> False;
          };
      };
    Seq e1 e2 -> match eq e1 (match eq e2 c) input;
    Alt e1 e2 ->
      case match eq e1 c input of {
        True -> True;
        False -> match eq e2 c input;
      };
    Rep e ->
      --match eq (Alt Empty (Seq e (Rep e))) c input;
      case c input of {
        True-> True;
        False ->
          match eq e (match eq (Rep e) c) input;
      };
  };

regRepTrue = \input ->  reg (Rep (Symb True)) input;
