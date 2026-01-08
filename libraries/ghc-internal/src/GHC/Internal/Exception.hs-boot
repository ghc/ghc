{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

{-
This SOURCE-imported hs-boot module cuts a big dependency loop:

         GHC.Internal.Exception
imports  GHC.Internal.Data.Maybe
imports  GHC.Internal.Base
imports  GHC.Internal.Err
imports  {-# SOURCE #-} GHC.Internal.Exception

More dramatically

         GHC.Internal.Exception
imports  GHC.Internal.Data.Typeable
imports  Data.Typeable.Internal
imports  GHC.Internal.Arr (fingerprint representation etc)
imports  GHC.Internal.Real
imports  {-# SOURCE #-} GHC.Internal.Exception

However, GHC.Exceptions loop-breaking exports are all nice,
well-behaved, non-bottom values.  The clients use 'raise#'
to get a visibly-bottom value.
-}

module GHC.Internal.Exception
  ( module GHC.Internal.Exception.Type
  , error, errorWithoutStackTrace, undefined
  ) where

import {-# SOURCE #-} GHC.Internal.Exception.Type
import GHC.Internal.Types ( Char, RuntimeRep, TYPE)
import GHC.Internal.Stack.Types ( HasCallStack )


error :: forall (r :: RuntimeRep). forall (a :: TYPE r).
         HasCallStack => [Char] -> a

errorWithoutStackTrace :: forall (r :: RuntimeRep). forall (a :: TYPE r).
                          [Char] -> a

undefined :: forall (r :: RuntimeRep). forall (a :: TYPE r).
             HasCallStack => a
