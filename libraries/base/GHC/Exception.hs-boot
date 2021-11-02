{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification#-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}

{-
This SOURCE-imported hs-boot module cuts a big dependency loop:

         GHC.Exception
imports  Data.Maybe
imports  GHC.Base
imports  GHC.Err
imports  {-# SOURCE #-} GHC.Exception

More dramatically

         GHC.Exception
imports  Data.Typeable
imports  Data.Typeable.Internal
imports  GHC.Arr (fingerprint representation etc)
imports  GHC.Real
imports  {-# SOURCE #-} GHC.Exception

However, GHC.Exceptions loop-breaking exports are all nice,
well-behaved, non-bottom values.  The clients use 'raise#'
to get a visibly-bottom value.
-}

module GHC.Exception
  ( module GHC.Exception.Type
  , errorCallException
  , errorCallWithCallStackException
  , throw
  ) where

import {-# SOURCE #-} GHC.Exception.Type
import GHC.Types ( Char, RuntimeRep, TYPE )
import GHC.Stack.Types ( CallStack, HasCallStack )

errorCallException :: [Char] -> SomeExceptionWithLocation
errorCallWithCallStackException :: [Char] -> CallStack -> SomeExceptionWithLocation

throw :: HasCallStack => forall (r :: RuntimeRep). forall (a :: TYPE r). forall e. Exception e => e -> a
