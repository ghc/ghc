{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}  

module T4497 where

norm2PropR a = twiddle (norm2 a) a

twiddle :: Normed a => a -> a -> Double  
twiddle a b = undefined

norm2 :: e -> RealOf e
norm2 = undefined

class (Num (RealOf t)) => Normed t

type family RealOf x
