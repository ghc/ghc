{-# language MagicHash,UnboxedSums #-}
module T22187 where
import GHC.Exts

foo :: (# Int64X2# | () #) -> ()
foo _ = ()
