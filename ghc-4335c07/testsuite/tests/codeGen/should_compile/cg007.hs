{-# LANGUAGE ForeignFunctionInterface #-}
module ShouldCompile where

import Foreign.C.Types

{-
During 6.11, this was failing like this:

In file included from /ghc/includes/Stg.h:207,

                 from /tmp/ghc2904_0/ghc2904_0.hc:3:0:
/tmp/ghc2904_0/ghc2904_0.hc: In function `swM_ret':

/tmp/ghc2904_0/ghc2904_0.hc:22:0:
     error: `gamma' undeclared (first use in this function)

/tmp/ghc2904_0/ghc2904_0.hc:22:0:
     error: (Each undeclared identifier is reported only once

/tmp/ghc2904_0/ghc2904_0.hc:22:0:
     error: for each function it appears in.)
-}

foreign import ccall unsafe "math.h gamma"
    gamma :: CDouble -> CDouble

