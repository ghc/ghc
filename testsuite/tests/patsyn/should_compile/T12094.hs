{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
module T12094 where

import GHC.Exts (Int#)

pattern Zero :: Int# -- commenting out this line works
pattern Zero <- 0#
