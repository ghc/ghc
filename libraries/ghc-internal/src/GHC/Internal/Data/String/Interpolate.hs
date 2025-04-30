{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Data.String.Interpolate
-- Copyright   :  (c) The University of Glasgow 2024
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The machinery behind -XStringInterpolation
--
-----------------------------------------------------------------------------

module GHC.Internal.Data.String.Interpolate (
  Interpolate (..),
) where

import GHC.Internal.Base
import GHC.Internal.Data.Either (Either (..))
import GHC.Internal.Data.List (intercalate)
import GHC.Internal.Show (show)

-- | @Interpolate a s@ allows a value of type @a@ to be interpolated
-- into a string interpolation of type @s@.
class Interpolate a s where
  interpolate :: a -> s

instance Interpolate String String where
  interpolate = id
instance Interpolate Char String where
  interpolate = interpolate . (:[])

instance Interpolate Int String where
  interpolate = show
instance Interpolate Double String where
  interpolate = show
instance Interpolate Bool String where
  interpolate = show

instance {-# OVERLAPPABLE #-}
  ( Interpolate a String
  ) => Interpolate [a] String where
  interpolate as = "[" ++ (intercalate ", " . map interpolate) as ++ "]"
instance
  ( Interpolate a String
  , Interpolate b String
  ) => Interpolate (Either a b) String where
  interpolate (Left a) = "Left " ++ interpolate a
  interpolate (Right b) = "Right " ++ interpolate b

instance
  ( Interpolate a String
  , Interpolate b String
  ) => Interpolate (a, b) String where
  interpolate (a, b) =
    mconcat
      [ "("
      , interpolate a
      , ", "
      , interpolate b
      , ")"
      ]
instance
  ( Interpolate a String
  , Interpolate b String
  , Interpolate c String
  ) => Interpolate (a, b, c) String where
  interpolate (a, b, c) =
    mconcat
      [ "("
      , interpolate a
      , ", "
      , interpolate b
      , ", "
      , interpolate c
      , ")"
      ]
