module X86.Cond (
        Cond(..),
        condUnsigned,
        condToSigned,
        condToUnsigned,
        maybeFlipCond
)

where

data Cond
        = ALWAYS        -- What's really used? ToDo
        | EQQ
        | GE
        | GEU
        | GTT
        | GU
        | LE
        | LEU
        | LTT
        | LU
        | NE
        | NEG
        | POS
        | CARRY
        | OFLO
        | PARITY
        | NOTPARITY
        deriving Eq

condUnsigned :: Cond -> Bool
condUnsigned GU  = True
condUnsigned LU  = True
condUnsigned GEU = True
condUnsigned LEU = True
condUnsigned _   = False


condToSigned :: Cond -> Cond
condToSigned GU  = GTT
condToSigned LU  = LTT
condToSigned GEU = GE
condToSigned LEU = LE
condToSigned x   = x


condToUnsigned :: Cond -> Cond
condToUnsigned GTT = GU
condToUnsigned LTT = LU
condToUnsigned GE  = GEU
condToUnsigned LE  = LEU
condToUnsigned x   = x

-- | @maybeFlipCond c@ returns @Just c'@ if it is possible to flip the
-- arguments to the conditional @c@, and the new condition should be @c'@.
maybeFlipCond :: Cond -> Maybe Cond
maybeFlipCond cond  = case cond of
        EQQ   -> Just EQQ
        NE    -> Just NE
        LU    -> Just GU
        GU    -> Just LU
        LEU   -> Just GEU
        GEU   -> Just LEU
        LTT   -> Just GTT
        GTT   -> Just LTT
        LE    -> Just GE
        GE    -> Just LE
        _other -> Nothing
