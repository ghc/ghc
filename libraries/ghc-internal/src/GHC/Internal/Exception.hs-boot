{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
  , errorCallException
  , errorCallWithCallStackException
  ) where

import {-# SOURCE #-} GHC.Internal.Exception.Type
import GHC.Internal.Types ( Char )
import GHC.Internal.Stack.Types ( CallStack )

errorCallException :: [Char] -> SomeException
errorCallWithCallStackException :: [Char] -> CallStack -> SomeException
