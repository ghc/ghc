module ShouldSucceed where

data OL a = MkOL [a]
data FG a b = MkFG (OL (a,b))
data AFE n a b = MkAFE (OL (n,(FG a b)))

--ranOAL :: OL (a,v) -> [a]
ranOAL :: OL (a,v) -> [v]
ranOAL (MkOL xs) = mAp sNd xs

mAp f [] = []
mAp f (x:xs) = (f x) : mAp f xs

sNd (f,s) = s

ranAFE :: AFE n a b -> [FG a b]  -- ?
ranAFE (MkAFE nfs) = ranOAL nfs




