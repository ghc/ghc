{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
imports  Data.Typeable.Internals
imports  GHC.Arr (fingerprint representation etc)
imports  GHC.Real
imports  {-# SOURCE #-} GHC.Exception

However, GHC.Exceptions loop-breaking exports are all nice,
well-behaved, non-bottom values.  The clients use 'raise#'
to get a visibly-bottom value.
-}

module GHC.Exception ( SomeException, errorCallException,
                       errorCallWithCallStackException,
                       divZeroException, overflowException, ratioZeroDenomException
    ) where
import GHC.Types ( Char )
import GHC.Stack.Types ( CallStack )

data SomeException
divZeroException, overflowException, ratioZeroDenomException  :: SomeException

errorCallException :: [Char] -> SomeException
errorCallWithCallStackException :: [Char] -> CallStack -> SomeException
