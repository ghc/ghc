{-# LANGUAGE Modifiers #-}
module NativePercentArrowDuplicate where
import GHC.Exts (M, U)
type Bad = Int %M %U -> Bool
