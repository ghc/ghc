module HybU ( hybrid_connected_components )
where

import Data.Array.Parallel.Unlifted

enumerate :: UArr Bool -> UArr Int
{-# INLINE enumerate #-}
enumerate = scanU (+) 0 . mapU (\b -> if b then 1 else 0)

pack_index :: UArr Bool -> UArr Int
{-# INLINE pack_index #-}
pack_index bs = mapU fstS . filterU sndS $ zipU (enumFromToU 0 (lengthU bs - 1))
                                                bs

shortcut_all :: UArr Int -> UArr Int
shortcut_all p = let pp = bpermuteU p p
                 in if p == pp then pp else shortcut_all pp

compress_graph :: UArr Int -> UArr (Int :*: Int)
               -> UArr (Int :*: Int) :*: UArr Int
compress_graph p e =
  let e1 :*: e2     = unzipU e
      e'            = zipU (bpermuteU p e1) (bpermuteU p e2)
      e''           = mapU (\(i :*: j) -> if i > j then j :*: i else i :*: j)
                    . filterU (\(i :*: j) -> i /= j)
                    $ e'

      roots         = zipWithU (==) p (enumFromToU 0 (lengthU p - 1))
      labels        = enumerate roots
      e1'' :*: e2'' = unzipU e''
      e'''          = zipU (bpermuteU labels e1'') (bpermuteU labels e2'')
  in
  e''' :*:  pack_index roots

hybrid_connected_components :: UArr (Int :*: Int) -> Int -> Int :*: UArr Int
{-# NOINLINE hybrid_connected_components #-}
hybrid_connected_components e n
  | nullU e   = 0 :*: enumFromToU 0 (n-1)
  | otherwise = let p        = shortcut_all
                             $ updateU (enumFromToU 0 (n-1)) e
                    e' :*: i = compress_graph p e
                    k :*: r  = hybrid_connected_components e' (lengthU i)
                    ins      = updateU p
                             . zipU i
                             $ bpermuteU i r
                in
                k+1 :*: bpermuteU ins ins

             
