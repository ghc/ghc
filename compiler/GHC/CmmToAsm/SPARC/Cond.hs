module GHC.CmmToAsm.SPARC.Cond (
        Cond(..),
)

where

import GhcPrelude

-- | Branch condition codes.
data Cond
        = ALWAYS
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
        | NEVER
        | POS
        | VC
        | VS
        deriving Eq
