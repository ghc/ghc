{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Control.Monad.Fix
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Monadic fixpoints.
--
-- For a detailed discussion, see Levent Erkok's thesis,
-- /Value Recursion in Monadic Computations/, Oregon Graduate Institute, 2002.
--

module Control.Monad.Fix
    (MonadFix(mfix),
     fix
     ) where

import GHC.Internal.Control.Monad.Fix
