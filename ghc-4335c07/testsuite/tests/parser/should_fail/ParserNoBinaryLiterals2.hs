{-# LANGUAGE MagicHash #-}

module ParserNoBinaryLiterals2 where

import GHC.Types

f :: Word -> ()
f (W# 0b0##) = ()
f _          = ()
