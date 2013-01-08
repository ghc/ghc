{-# LANGUAGE AllowAmbiguousTypes, TypeFamilies #-}
-- Type of x is ambiguous

module T2693 where

type family TFn a :: *

-- Boiled down version
f :: Maybe ()
f = do
  let Just x = undefined :: Maybe (TFn a)
  let n = fst x + fst x
  return ()

f2 :: Maybe ()
f2 = do
  let x :: TFn a
      x = undefined 
  let n = fst x + snd x
  return ()

-- Close to the original report
data PVR a = PVR {pvrX :: Int} deriving (Eq)

g :: () -> Maybe (TFn a)
g _ = undefined

h :: Maybe ()
h = do pvs <- mapM g undefined
       let n = (map pvrX pvs) `min` (map pvrX pvs)
       undefined
