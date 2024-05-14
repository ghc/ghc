{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.IO.Class
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  stable
-- Portability :  portable
--
-- Class of monads based on @IO@.
-----------------------------------------------------------------------------

module Control.Monad.IO.Class
  ( MonadIO(..) )
  where

import GHC.Internal.Control.Monad.IO.Class
