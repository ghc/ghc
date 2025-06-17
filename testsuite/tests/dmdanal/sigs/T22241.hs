{-# LANGUAGE GHC2021 #-}
module T22241 where

data D = D { unD :: !Int }

-- We should unbox y here, which only happens if DmdAnal sees that $WD will
-- unbox it.
f :: Bool -> Int -> D
f x y = D (go x)
  where
    go False = y
    go True  = go False
{-# NOINLINE f #-}



data T a = T Int !a
get (T _ x) = x

-- Here, the goal is to discard `unD (f True z)` and thus `z` as absent by
-- looking through $WT in `j` *during the first pass of DmdAnal*!
g :: Bool -> Int -> Int -> Bool
g x y z | even y    = get (fst t)
        | y > 13    = not (get (fst t))
        | otherwise = False
  where
    t | x         = j (unD (f True z))
      | otherwise = j (unD (f False z))
      where
        j a = (T a x, True)
        {-# NOINLINE j #-}
{-# NOINLINE g #-}
