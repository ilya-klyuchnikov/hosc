-- partial ackermann
-- good: HOSC, WithCC

data Nat = Z | S Nat;

ack (S (S Z)) x where

ack = \m n -> case m of {
        Z -> S n;
        S m1 -> case n of {
                Z -> ack m1 (S Z);
                S n1 -> ack m1 (ack m n1);
        };
};