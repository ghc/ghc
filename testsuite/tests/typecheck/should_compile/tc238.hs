-- This innocuous module made GHC 6.6 have exponential behaviour
-- when doing validity checking on the synonym declarations
--
-- This lot is enough to make the test time out, I hope

module ShouldCompile where

data TIACons1 i r c = K (c i) (r c)

type TIACons2  t x = TIACons1 t (TIACons1 t x)
type TIACons3  t x = TIACons2 t (TIACons1 t x)
type TIACons4  t x = TIACons2 t (TIACons2 t x)
type TIACons7  t x = TIACons4 t (TIACons3 t x)
type TIACons8  t x = TIACons4 t (TIACons4 t x)
type TIACons15 t x = TIACons8 t (TIACons7 t x)
type TIACons16 t x = TIACons8 t (TIACons8 t x)
type TIACons31 t x = TIACons16 t (TIACons15 t x)
type TIACons32 t x = TIACons16 t (TIACons16 t x)
type TIACons47 t x = TIACons32 t (TIACons15 t x)
type TIACons48 t x = TIACons32 t (TIACons16 t x)
