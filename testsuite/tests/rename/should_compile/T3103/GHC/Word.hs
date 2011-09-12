{-# LANGUAGE NoImplicitPrelude, MagicHash #-}

module GHC.Word (
    Word(..),
    ) where

import GHC.Base
import GHC.Num
import {-# SOURCE #-} GHC.Unicode ()
import GHC.Show (Show(..))
import GHC.Integer

data Word = W# Word# deriving Eq

instance Show Word where

instance Num Word where
    (W# x#) + (W# y#)      = W# (x# `plusWord#` y#)
    (W# x#) - (W# y#)      = W# (x# `minusWord#` y#)
    (W# x#) * (W# y#)      = W# (x# `timesWord#` y#)
    negate (W# x#)         = W# (int2Word# (negateInt# (word2Int# x#)))
    abs x                  = x
    signum 0               = 0
    signum _               = 1
    fromInteger i          = W# (integerToWord i)

