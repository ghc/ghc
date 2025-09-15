{-# LANGUAGE MagicHash #-}

module T12531 where
import GHC.Exts

f x = I# (_ +# x)
