{-# LANGUAGE MagicHash #-}
module T18291 where

import GHC.Magic

hi :: Int
hi = runRW# $ \_ -> 42
