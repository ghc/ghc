{-# LANGUAGE AllowAmbiguousTypes, TypeFamilies #-}

module T5837 where

type family TF a :: *
type instance TF (a,b) = (TF a, TF b)

t :: (a ~ TF (a,Int)) => Int
t = undefined

{-

         [G] a ~ TF (a,Int)               -- a = a_am1
-->
         [G] TF (a,Int) ~ fsk             -- fsk = fsk_am8
inert    [G] fsk ~ a

--->
    [G] fsk ~ (TF a, TF Int)
inert    [G] fsk ~ a

--->
    a ~ (TF a, TF Int)
inert    [G] fsk ~ a

---> (attempting to flatten (TF a) so that it does not mention a
         TF a ~ fsk2
inert    a ~ (fsk2, TF Int)
inert    fsk ~ (fsk2, TF Int)

---> (substitute for a)
         TF (fsk2, TF Int) ~ fsk2
inert    a ~ (fsk2, TF Int)
inert    fsk ~ (fsk2, TF Int)

---> (top-level reduction, re-orient)
         fsk2 ~ (TF fsk2, TF Int)
inert    a ~ (fsk2, TF Int)
inert    fsk ~ (fsk2, TF Int)

---> (attempt to flatten (TF fsk2) to get rid of fsk2
         TF fsk2 ~ fsk3
         fsk2 ~ (fsk3, TF Int)
inert    a   ~ (fsk2, TF Int)
inert    fsk ~ (fsk2, TF Int)

--->
         TF fsk2 ~ fsk3
inert    fsk2 ~ (fsk3, TF Int)
inert    a   ~ ((fsk3, TF Int), TF Int)
inert    fsk ~ ((fsk3, TF Int), TF Int)

-}