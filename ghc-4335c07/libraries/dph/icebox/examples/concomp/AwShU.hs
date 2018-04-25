module AwShU ( aw_connected_components )
where

import Data.Array.Parallel.Unlifted

starCheck :: UArr Int -> UArr Bool
starCheck ds =
  let gs  = bpermuteU ds ds
      st  = zipWithU (==) ds gs
      st' = updateU st . filterU (not . sndS)
                       $ zipU gs st
  in
  bpermuteU st' gs

conComp :: UArr Int -> UArr (Int :*: Int) -> Int :*: UArr Int
conComp ds es =
  let es1 :*: es2 = unzipU es
      ds'         = updateU ds
                  . mapU (\(di :*: dj :*: gi) -> (di :*: dj))
                  . filterU (\(di :*: dj :*: gi) -> gi == di && di > dj)
                  $ zip3U (bpermuteU ds es1)
                          (bpermuteU ds es2)
                          (bpermuteU ds (bpermuteU ds es1))
      ds''        = updateU ds'
                  . mapU (\(di :*: dj :*: st) -> (di :*: dj))
                  . filterU (\(di :*: dj :*: st) -> st && di /= dj)
                  $ zip3U (bpermuteU ds' es1)
                          (bpermuteU ds' es2)
                          (bpermuteU (starCheck ds') es1)
  in
  if andU (starCheck ds'')
    then 1 :*: ds''
    else rec $ conComp (bpermuteU ds'' ds'') es
  where
    rec (n :*: arr) = n+1 :*: arr

aw_connected_components :: UArr (Int :*: Int) -> Int -> Int :*: UArr Int
{-# NOINLINE aw_connected_components #-}
aw_connected_components es n =
  let ds  = enumFromToU 0 (n-1) +:+ enumFromToU 0 (n-1)
      es' = es +:+ mapU (\(j :*: i) -> i :*: j) es
      r :*: cs = conComp ds es'
  in
  r :*: cs

