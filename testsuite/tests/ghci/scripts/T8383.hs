{-# LANGUAGE MagicHash #-}
module T8383 where
import GHC.Exts
f :: Int# -> Int#
f x = x
foo = print $ (tagToEnum# (f 0#) :: Bool)
bar = print $ (tagToEnum# (f 1#) :: Bool)

