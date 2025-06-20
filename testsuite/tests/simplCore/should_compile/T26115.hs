module T26115 where

class C a b where { op1, op2 :: a -> b -> Bool
                  ; op2 = op1 }

instance C Bool b where { op1 _ _ = True }

instance C p q => C [p] q where
  op1 [x] y = op1 x y
  {-# SPECIALISE instance C [Bool] b #-}
