{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
module Bug where

import GHC.Exts
import Unsafe.Coerce

f :: Int -> Int
f x = unsafeCoerce# @LiftedRep @LiftedRep @Int @Int x

