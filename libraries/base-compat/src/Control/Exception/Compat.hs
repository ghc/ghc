{-# LANGUAGE CPP, NoImplicitPrelude #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
#if MIN_VERSION_base(4,10,0)
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
#endif
module Control.Exception.Compat (
  module Base
, throw
) where

import Control.Exception as Base
#if MIN_VERSION_base(4,10,0) && !(MIN_VERSION_base(4,12,0))
  hiding (throw)
import GHC.Exts (RuntimeRep, TYPE, raise#)

-- | Throw an exception.  Exceptions may be thrown from purely
-- functional code, but may only be caught within the 'IO' monad.
throw :: forall (r :: RuntimeRep). forall (a :: TYPE r). forall e.
         Exception e => e -> a
throw e = raise# (toException e)
#endif
