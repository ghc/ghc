{-# LANGUAGE MagicHash, Strict #-}
{-# OPTIONS_GHC -Wredundant-bang-patterns #-}

import GHC.Prim (Int#)

idInt# :: Int# -> Int#
idInt# x = x

main = pure ()
