{-# LANGUAGE TypeFamilies #-}
module T4093a where

type family Foo x
type instance Foo () = Maybe ()

hang :: (Foo e ~ Maybe e) => Foo e
hang = Just ()


{- Ambiguity check

 [G] Foo e ~ Maybe e
 [W] Foo e ~ Foo ee
 [W] Foo ee ~ Maybe ee)
---
 [G] Foo e ~ fsk
 [G] fsk ~ Maybe e

 [W] Foo e ~ fmv1
 [W] Foo ee ~ fmv2
 [W] fmv1 ~ fmv2
 [W] fmv2 ~ Maybe ee

--->   fmv1 := fsk
 [W] Foo ee ~ fmv2
 [W] fsk ~ fmv2
 [W] fmv2 ~ Maybe ee

--->
 [W] Foo ee ~ fmv2
 [W] fmv2 ~ Maybe e
 [W] fmv2 ~ Maybe ee

-}