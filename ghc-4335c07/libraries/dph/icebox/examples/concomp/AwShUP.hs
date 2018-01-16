module AwShUP ( aw_connected_components )
where

import Data.Array.Parallel.Unlifted
import Data.Array.Parallel.Unlifted.Parallel

starCheck :: UArr Int -> UArr Bool
starCheck ds =
  let gs  = bpermuteUP ds ds
      st  = zipWithUP (==) ds gs
      st' = updateUP st . filterUP (not . sndS)
                        $ zipU gs st
  in
  bpermuteUP st' gs

conComp :: UArr Int -> UArr (Int :*: Int) -> Int :*: UArr Int
conComp ds es =
  let es1 :*: es2 = unzipU es
      ds'         = updateUP ds
                  . mapUP (\(di :*: dj :*: gi) -> (di :*: dj))
                  . filterUP (\(di :*: dj :*: gi) -> gi == di && di > dj)
                  $ zip3U (bpermuteUP ds es1)
                          (bpermuteUP ds es2)
                          (bpermuteUP ds (bpermuteUP ds es1))
      ds''        = updateUP ds'
                  . mapUP (\(di :*: dj :*: st) -> (di :*: dj))
                  . filterUP (\(di :*: dj :*: st) -> st && di /= dj)
                  $ zip3U (bpermuteUP ds' es1)
                          (bpermuteUP ds' es2)
                          (bpermuteUP (starCheck ds') es1)
  in
  if andUP (starCheck ds'')
    then 1 :*: ds''
    else rec $ conComp (bpermuteUP ds'' ds'') es
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

