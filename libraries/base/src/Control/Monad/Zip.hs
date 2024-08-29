{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Zip
-- Copyright   :  (c) Nils Schweinsberg 2011,
--                (c) George Giorgidze 2011
--                (c) University Tuebingen 2011
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Monadic zipping (used for monad comprehensions)
--
-----------------------------------------------------------------------------

module Control.Monad.Zip ( MonadZip(..) ) where

import GHC.Internal.Control.Monad.Zip(MonadZip(..))
