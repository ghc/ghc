{-# OPTIONS_GHC -fno-warn-missing-methods -fno-warn-deprecations #-}
module C where

 data Complex a = !a :+ !a
     deriving (Eq, Show)

 instance  (RealFloat a) => Num (Complex a)  where
     {-# SPECIALISE instance Num (Complex Float) #-}
     {-# SPECIALISE instance Num (Complex Double) #-}
     (x :+ y) + (x' :+ y')   =  (x + x') :+ (y + y')
