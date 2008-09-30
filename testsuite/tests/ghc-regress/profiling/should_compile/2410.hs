{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module C where

 data RealFloat a => Complex a = !a :+ !a
     deriving (Eq, Show)

 instance  (RealFloat a) => Num (Complex a)  where
     {-# SPECIALISE instance Num (Complex Float) #-}
     {-# SPECIALISE instance Num (Complex Double) #-}
     (x :+ y) + (x' :+ y')   =  (x + x') :+ (y + y')
