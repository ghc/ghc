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
[G] UnF (F b) ~ b

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

--> rewrite (A) with (B), and match with g2

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



{-

-----
Inert: [G] fsk_amA ~ b_amr
       [G] UnF fsk_amy ~ fsk_amA
       [G} F b_amr ~ fsk_amy

wl: [W] F b_amr ~ F b_amt

work item: [W] UnF (F b_amt) ~ b_amt
  b_amt is the unification variable

===>      b_amt := s_amF

Inert: [G] fsk_amA ~ b_amr
       [G] UnF fsk_amy ~ fsk_amA
       [G} F b_amr ~ fsk_amy

wl: [W] F b_amr ~ F b_amt
    [W] UnF s_amD ~ s_amF

work item: [W] F b_amt ~ s_amD


===>
wl: [W] F b_amr ~ F b_amt
    [W] UnF s_amD ~ s_amF

Inert: [G] fsk_amA ~ b_amr
       [G] UnF fsk_amy ~ fsk_amA
       [G} F b_amr ~ fsk_amy
       [W] F s_amF ~ s_amD

===>
wl: [W] F b_amr ~ F b_amt

Inert: [G] fsk_amA ~ b_amr
       [G] UnF fsk_amy ~ fsk_amA
       [G} F b_amr ~ fsk_amy
       [W] F s_amF ~ s_amD
       [W] UnF s_amD ~ s_amF

===>
Inert: [G] fsk_amA ~ b_amr
       [G] UnF fsk_amy ~ fsk_amA
       [G} F b_amr ~ fsk_amy
       [W] UnF s_amD ~ s_amF
       [W] F s_amF ~ s_amD

wl:

work-item: [W] F b_amr ~ F b_amt
--> fsk_amy ~ s_amD
--> s_amD ~ fsk_amy

===>
Inert: [G] fsk_amA ~ b_amr
       [G] UnF fsk_amy ~ fsk_amA
       [G} F b_amr ~ fsk_amy
       [W] UnF s_amD ~ s_amF
       [W] F s_amF ~ s_amD
       [W] s_amD ~ fsk_amy

wl:

work item: [D] UnF s_amD ~ s_amF

--> [D] UnF fsk_amy ~ s_amF
--> [D] s_amF ~ fsk_amA

===>
Inert: [G] fsk_amA ~ b_amr
       [G] UnF fsk_amy ~ fsk_amA
       [G} F b_amr ~ fsk_amy
       [W] UnF s_amD ~ s_amF
       [W] F s_amF ~ s_amD
       [W] s_amD ~ fsk_amy
       [D] s_amF ~ fsk_amA

wl:

work item: [D] F s_amF ~ s_amD
--> F fsk_amA ~ s_amD
--> s_amd ~ b_amr
-}
