module X86.Cond (
        Cond(..),
        condUnsigned,
        condToSigned,
        condToUnsigned,
        maybeFlipCond,
        maybeInvertCond
)

where

import GhcPrelude

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

-- | If we apply @maybeInvertCond@ to the condition of a jump we turn
-- jumps taken into jumps not taken and vice versa.
--
-- Careful! If the used comparison and the conditional jump
-- don't match the above behaviour will NOT hold.
-- When used for FP comparisons this does not consider unordered
-- numbers.
-- Also inverting twice might return a synonym for the original condition.
maybeInvertCond :: Cond -> Maybe Cond
maybeInvertCond cond  = case cond of
        ALWAYS  -> Nothing
        EQQ     -> Just NE
        NE      -> Just EQQ

        NEG     -> Just POS
        POS     -> Just NEG

        GEU     -> Just LU
        LU      -> Just GEU

        GE      -> Just LTT
        LTT     -> Just GE

        GTT     -> Just LE
        LE      -> Just GTT

        GU      -> Just LEU
        LEU     -> Just GU

        --GEU "==" NOTCARRY, they are synonyms
        --at the assembly level
        CARRY   -> Just GEU

        OFLO    -> Nothing

        PARITY  -> Just NOTPARITY
        NOTPARITY -> Just PARITY
