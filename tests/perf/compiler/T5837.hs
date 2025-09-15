{-# LANGUAGE AllowAmbiguousTypes, TypeFamilies #-}

module T5837 where

import Data.Kind

type family TF a :: Type
type instance TF (a,b) = (TF a, TF b)

t :: (a ~ TF (a,Int)) => Int
t = undefined

{-

         [G] a ~ TF (a,Int)               -- a = a_am1
-->
         [G] TF (a,Int) ~ fsk             -- fsk = fsk_am8

inert    [G] fsk ~ a

---> reduce
         [G] fsk ~ (TF a, TF Int)

inert    [G] fsk ~ a

---> substitute for fsk and flatten
         [G] TF a ~ fsk1
         [G] TF Int ~ fsk2

inert    [G] fsk ~ a
         [G] a ~ (fsk1, fsk2)

---> (substitute for a in first constraint)
         TF (fsk1, fsk2) ~ fsk1     (C1)
         TF Int ~ fsk2

inert    a ~ (fsk2, TF Int)
inert    fsk ~ (fsk2, TF Int)


------- At this point we are stuck because of
--      the recursion in the first constraint C1
--      Hooray

-- Before, we reduced C1, which led to a loop

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
