module Ppr009 where


{-# INLINE strictStream #-}
strictStream (Bitstream l v)
    = {-# CORE "Strict Bitstream stream" #-}
      S.concatMap stream (GV.stream v)
      `S.sized`
      Exact l
