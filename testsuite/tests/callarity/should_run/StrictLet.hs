{-# LANGUAGE MagicHash #-}

{-
If the (unboxed, hence strict) "let thunk =" would survive to the CallArity
stage, it might yield wrong results (eta-expanding thunk and hence "cond" would
be called multiple times).

It does not actually happen (CallArity sees a "case"), so this test just
safe-guards against future changes here.
-}

import Debug.Trace
import GHC.Exts
import System.Environment

cond :: Int# -> Bool
cond x = trace ("cond called with " ++ show (I# x)) True
{-# NOINLINE cond #-}


bar (I# x) =
    let go n = let x = thunk n
               in case n of
                    100# -> I# x
                    _    -> go (n +# 1#)
    in go x
  where thunk = if cond x then \x -> (x +# 1#) else \x -> (x -# 1#)


main = do
    args <- getArgs
    bar (length args) `seq` return ()
