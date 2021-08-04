{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TypeInType #-}
#endif
module Basement.Error
    ( error
    ) where

import           GHC.Prim
import           Basement.UTF8.Base
import           Basement.Compat.CallStack

#if MIN_VERSION_base(4,9,0)

import           GHC.Types (RuntimeRep)
import           GHC.Exception (errorCallWithCallStackException)

-- | stop execution and displays an error message
error :: forall (r :: RuntimeRep) . forall (a :: TYPE r) . HasCallStack => String -> a
error s = raise# (errorCallWithCallStackException (sToList s) ?callstack)

#elif MIN_VERSION_base(4,7,0)

import           GHC.Exception (errorCallException)

error :: String -> a
error s = raise# (errorCallException (sToList s))

#else

import           GHC.Types
import           GHC.Exception

error :: String -> a
error s = throw (ErrorCall (sToList s))

#endif
