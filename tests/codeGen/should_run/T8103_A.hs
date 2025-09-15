{-# LANGUAGE MagicHash #-}
module T8103_A where
import GHC.Exts

{-# NOINLINE foo #-}
foo  :: Double# -> Double# -> Double#
foo a b = (a +## b)
