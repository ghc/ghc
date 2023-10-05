{-# LANGUAGE TypeFamilies #-}
module T4093a where

type family Foo x
type instance Foo () = Maybe ()

hang :: (Foo e ~ Maybe e) => Foo e
hang = Just ()

-- Type is not ambiguous; should get a complaint
-- for (e ~ ()) arising from the Just ()

{- Ambiguity check

 [G] Foo e ~ Maybe e
 [W] Foo e ~ Foo e0
 [W] Foo e0 ~ Maybe e0
---
 [G] Foo e ~ fsk
 [G] fsk ~ Maybe e

 [W] Foo e ~ fmv1
 [W] Foo e0 ~ fmv2
 [W] fmv1 ~ fmv2
 [W] fmv2 ~ Maybe e0

--->   fmv1 := fsk
 [G] Foo e ~ fsk
 [G] fsk ~ Maybe e

 [W] Foo e0 ~ fmv2
 [W] fsk ~ fmv2
 [W] fmv2 ~ Maybe e0

--->
 [G] Foo e ~ fsk
 [G] fsk ~ Maybe e

 [W] Foo e0 ~ fmv2
 [W] fmv2 ~ Maybe e
 [W] fmv2 ~ Maybe e0

Now the question is whether we get a derived equality e ~ e0.  Currently
we don't, but we easily could.  But then we'd need to be careful not to
report insoluble Int ~ Bool if we had
   F a ~ Int, F a ~ Bool
-}
