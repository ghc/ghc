{-# LANGUAGE MagicHash #-}
module T18291 where

hi :: Int
hi = runRW# $ \_ -> 42
