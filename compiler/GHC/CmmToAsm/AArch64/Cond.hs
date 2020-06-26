module GHC.CmmToAsm.AArch64.Cond  where

import GHC.Prelude

import GHC.Utils.Panic

data Cond
    = ALWAYS -- b.al
    | EQ     -- b.eq
    | NE     -- b.ne
    -- signed
    | SLT    -- b.lt
    | SLE    -- b.le
    | SGE    -- b.ge
    | SGT    -- b.gt
    -- unsigned
    | ULT    -- b.lo
    | ULE    -- b.ls
    | UGE    -- b.hs
    | UGT    -- b.hi
    -- others
    | NEVER  -- ne
    | VS     -- oVerflow set
    | VC     -- oVerflow clear
    deriving Eq
