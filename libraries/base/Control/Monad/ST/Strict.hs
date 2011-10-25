{-# LANGUAGE CPP #-}
#if sh_SAFE_DEFAULT
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Unsafe #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.ST.Strict
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires universal quantification for runST)
--
-- The strict ST monad (re-export of "Control.Monad.ST")
--
-----------------------------------------------------------------------------

module Control.Monad.ST.Strict (
        module Control.Monad.ST
  ) where

#if sh_SAFE_DEFAULT
import safe Control.Monad.ST
#else
import Control.Monad.ST
#endif

