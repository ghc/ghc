module Main (mainPrimIO) where

import PreludePrimIO

mainPrimIO :: PrimIO ()
mainPrimIO
  = _ccall_ stg_getc (``stdin'' :: _Addr)
                  `thenPrimIO` \ (I# ch) ->
    if ch <# 0# then -- SIGH: ch ==# ``EOF''
        returnPrimIO ()
    else
        _ccall_ stg_putc (C# (chr# ch))
                  (``stdout'' :: _Addr)
                            `seqPrimIO`
        mainPrimIO

-- 1,737,897 bytes/sec ( 600KB input)
-- 1,808,993 bytes/sec ( 9.3MB input)
-- 1,711,850 bytes/sec (25.5MB input)
