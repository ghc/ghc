{-# LANGUAGE Modifiers, TypeOperators #-}
module NativePercentArrowWrong where
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (M, U)
bad :: (Int %M -> Bool) :~: (Int %U -> Bool)
bad = Refl
