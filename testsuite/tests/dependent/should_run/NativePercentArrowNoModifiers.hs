{-# LANGUAGE LinearTypes, NoModifiers #-}
module NativePercentArrowNoModifiers where
import GHC.Exts (U)
type Bad = Int %U -> Bool
