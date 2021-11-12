{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Dollar2 where
{-
inplace/bin/ghc-stage0 -O2 -dcore-lint
-}


import GHC.Base

data AB = A () | B ()

qux :: Bool
qux = True
{-# NOINLINE qux #-}

foo = id $ ((if qux then A else B) $ ())

{-

-}
