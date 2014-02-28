{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Coerce
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Safe coercions between data types.
--
-----------------------------------------------------------------------------

module Data.Coerce
        ( -- * Safe coercions
          coerce, Coercible,
        ) where
import GHC.Prim (coerce, Coercible)
