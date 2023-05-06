{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module T3651 where

data Z a where
  U :: Z ()
  B :: Z Bool

unsafe1 :: Z a -> Z a -> a
unsafe1 B U = ()

{- For unsafe1 we get:

     [G] a ~ () => [G] a ~ Bool => [W] Bool ~ a

By the time we get to the Wanted we have:
    inert:  [G] a ~# Bool    (CEqCan)
            [G] () ~# Bool   (CIrredCan)
    work: [W] Bool ~ a

We rewrite with the CEqCan to get [W] Bool ~ (); we reduce that
to [W] Bool ~# (). That is insoluble, but we solve it from [G] () ~# Bool
-}

unsafe2 :: a ~ b => Z b -> Z a -> a
unsafe2 B U = ()

unsafe3 :: a ~ b => Z a -> Z b -> a
unsafe3 B U = True
