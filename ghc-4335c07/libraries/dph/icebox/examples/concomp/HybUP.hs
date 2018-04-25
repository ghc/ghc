module HybUP ( hybrid_connected_components )
where

import Data.Array.Parallel.Unlifted
import Data.Array.Parallel.Unlifted.Parallel

enumerate :: UArr Bool -> UArr Int
{-# INLINE enumerate #-}
enumerate = scanUP (+) 0 . mapUP (\b -> if b then 1 else 0)

pack_index :: UArr Bool -> UArr Int
{-# INLINE pack_index #-}
pack_index bs = mapUP fstS
              . filterUP sndS
              $ zipU (enumFromToUP 0 (lengthU bs - 1))
                     bs

shortcut_all :: UArr Int -> UArr Int
shortcut_all p = let pp = bpermuteUP p p
                 in if p == pp then pp else shortcut_all pp

compress_graph :: UArr Int -> UArr (Int :*: Int)
               -> UArr (Int :*: Int) :*: UArr Int
compress_graph p e =
  let e1 :*: e2     = unzipU e
      e'            = zipU (bpermuteUP p e1) (bpermuteUP p e2)
      e''           = mapUP (\(i :*: j) -> if i > j then j :*: i else i :*: j)
                    . filterUP (\(i :*: j) -> i /= j)
                    $ e'

      roots         = zipWithUP (==) p (enumFromToUP 0 (lengthU p - 1))
      labels        = enumerate roots
      e1'' :*: e2'' = unzipU e''
      e'''          = zipU (bpermuteUP labels e1'') (bpermuteUP labels e2'')
  in
  e''' :*:  pack_index roots

hybrid_connected_components :: UArr (Int :*: Int) -> Int -> Int :*: UArr Int
{-# NOINLINE hybrid_connected_components #-}
hybrid_connected_components e n
  | nullU e   = 0 :*: enumFromToUP 0 (n-1)
  | otherwise = let p        = shortcut_all
                             $ updateUP (enumFromToUP 0 (n-1)) e
                    e' :*: i = compress_graph p e
                    k :*: r  = hybrid_connected_components e' (lengthU i)
                    ins      = updateUP p
                             . zipU i
                             $ bpermuteUP i r
                in
                k+1 :*: bpermuteUP ins ins

