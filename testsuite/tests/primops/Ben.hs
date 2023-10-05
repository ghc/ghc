
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Ben where

import Data.Kind
import GHC.Exts
import GHC.IO

foo :: forall {s} a. a -> State# s -> (# Int#, State# s #)
foo x s0 = keepAlive# x s0 (\s1 -> (# 42#, s1 #))
  --keepAlive#
