module Main (mainPrimIO) where

import PreludePrimIO

mainPrimIO :: PrimIO ()
mainPrimIO
  = _casm_
    ``do { int c;
    while ((c = getchar()) != EOF) {
        putchar(c);
    }} while (0);
    %r = 1;'' -- pretend we have a "result"
    `thenPrimIO` \ (I# _) ->
    returnPrimIO ()

-- 1,955,134 bytes/sec ( 600KB input)
-- 1,989,892 bytes/sec ( 9.3MB input)
-- 1,871,706 bytes/sec (25.5MB input)
