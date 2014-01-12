{-# OPTIONS -XGADTs #-}

module RepAux (
  toSpineRl
) where

data MTup l where
    P :: MTup l -> MTup (a,l)

data Spine a where
    S :: Spine (a -> b) -> Spine b

toSpineRl :: MTup l -> l -> (l -> a) -> Spine a 
toSpineRl (P rs) (a, l) into = S (toSpineRl rs l into')
  where 
    into' tl1 x1 = into (x1,tl1)
