module Main (mainPrimIO) where

import PreludePrimIO

mainPrimIO :: PrimIO ()
mainPrimIO
 = copy (``stdin'' :: _FILE)
        (``stdout'' :: _FILE)
 where
  copy inn out
   = fread 1 4096 inn
         `thenPrimIO` \ (n, s) ->
     if n <= 0
     then returnPrimIO ()
     else fwrite s 1 n out `seqPrimIO`
          copy inn out

-- 4,170,953 bytes/sec ( 600KB input)
-- 7,993,583 bytes/sec ( 9.3MB input)
-- 6,917,175 bytes/sec (25.5MB input)
