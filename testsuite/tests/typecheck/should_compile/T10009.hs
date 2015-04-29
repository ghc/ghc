{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module T10009 where


type family F a
type family UnF a

f :: (UnF (F b) ~ b) => F b -> ()
f = error "urk"

g :: forall a. (UnF (F a) ~ a) => a -> ()
g _ = f (undefined :: F a)


{- ---------------
[G] UnF (F a) ~ a

[W] UnF (F beta) ~ beta
[W] F a ~ F beta

-------------------
[G] g1: F a ~ fsk1         fsk1 := F a
[G] g2: UnF fsk1 ~ fsk2    fsk2 := UnF fsk1
[G] g3: fsk2 ~ a

[W] w1: F beta ~ fmv1
[W] w2: UnF fmv1 ~ fmv2
[W] w3: fmv2 ~ beta
[W] w5: fsk1 ~ fmv1   -- From F a ~ F beta
                      -- using flat-cache

---- No progress in solving -----
-- Unflatten:

[W] w3: UnF (F beta) ~ beta
[W] w5: fsk1 ~ F beta

--- Improvement

[D] F beta ~ fmv1
[D] UnF fmv1 ~ fmv2    -- (A)
[D] fmv2 ~ beta
[D] fmv1 ~ fsk1        -- (B) From F a ~ F beta
                       -- NB: put fmv on left

--> rewrite (A) with (B), and metch with g2

[D] F beta ~ fmv1
[D] fmv2 ~ fsk2        -- (C)
[D] fmv2 ~ beta        -- (D)
[D] fmv1 ~ fsk1

--> rewrite (D) with (C) and re-orient

[D] F beta ~ fmv1
[D] fmv2 ~ fsk2
[D] beta ~ fsk2       -- (E)
[D] fmv1 ~ fsk1

-- Now we can unify beta!
-}
