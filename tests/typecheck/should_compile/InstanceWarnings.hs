{-# LANGUAGE GADTs #-}
module InstanceWarnings where
import InstanceWarnings_aux

data Sel a where
    S1 :: Sel Int
    S2 :: Sel Bool
    S3 :: Sel Char

f :: T a -> Sel a -> String
f t S1 = show t
f t S2 = show t
f t S3 = if t == T2 then show t else ""