data RE a =
    RE_ZERO	String          --' L(0)   = {} (empty set, or failure with message)
    | RE_UNIT               --' L(1)   = { [] } (empty sequence, or success)
    | RE_SYM a              --' L(x)   = { [x] }
    | RE_DOT                --' accept any single symbol
    | RE_REP (RE a)         --' L(e*)  = { [] } `union` L(e+)
    | RE_PLUS (RE a)        --' L(e+)  = { x ++ y | x <- L(e), y <- L(e*) }
    | RE_OPT (RE a)         --' L(e?)  = L(e) `union` { [] }
    | RE_SEQ (RE a) (RE a)  --' L(e,f) = { x ++ y | x <- L(e), y <- L(f) }
    | RE_ALT (RE a) (RE a)  --' L(e|f) = L(e) `union` L(f)
    deriving (Show, Eq)
    
re1 = RE_REP (RE_ALT RE_UNIT (RE_SYM 'a'))
re2 = RE_SEQ (RE_SYM 'b') (RE_REP (RE_SYM 'a'))
    
delta :: (Eq a, Show a) => RE a -> a -> RE a
delta re s = case re of
    RE_ZERO m        -> RE_ZERO m
    RE_UNIT          -> RE_ZERO ("Symbol "++ show s ++" unexpected.")
    RE_SYM sym
        | s == sym   -> RE_UNIT
        | otherwise  -> RE_ZERO ("Symbol "++ show sym ++" expected, but "++
                                 "symbol "++ show s ++" found.")
    RE_REP e         -> RE_SEQ (delta e s) (RE_REP e)
    RE_PLUS e        -> RE_SEQ (delta e s) (RE_REP e)
    RE_OPT e         -> delta e s
    RE_SEQ e f
        | nullable e -> RE_ALT (RE_SEQ (delta e s) f) (delta f s)
        | otherwise  -> RE_SEQ (delta e s) f
    RE_ALT e f       -> RE_ALT (delta e s) (delta f s)
    RE_DOT           -> RE_UNIT
    
matches :: (Eq a, Show a) => RE a -> [a] -> RE a
matches e = foldl delta e

nullable ::  (Show a) => RE a -> Bool
nullable (RE_ZERO _)  = False
nullable RE_UNIT      = True
nullable (RE_SYM _)   = False
nullable (RE_REP _)   = True
nullable (RE_PLUS e)  = nullable e
nullable (RE_OPT _)   = True
nullable (RE_SEQ e f) = nullable e && nullable f
nullable (RE_ALT e f) = nullable e || nullable f
nullable RE_DOT       = True

checkRE :: (Show a) => RE a -> String
checkRE (RE_ZERO m) = m
checkRE re
    | nullable re = "MATCHED"
    | otherwise   = "Input must match " ++ (show re)