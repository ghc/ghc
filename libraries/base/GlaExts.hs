module GlaExts (module GHC.Exts, Addr(..)) where
import GHC.Exts

data Addr = A# Addr# 	deriving (Eq, Ord)
