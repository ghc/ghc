{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Either
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The Either type, and associated operations.
--
-----------------------------------------------------------------------------

module Data.Either (
   Either(..),
   either	-- :: (a -> c) -> (b -> c) -> Either a b -> c
 ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Base

{-|

The 'Either' type represents values with two possibilities: a value of
type @'Either' a b@ is either @'Left' a@ or @'Right' b@.

The 'Either' type is sometimes used to represent a value which is
either correct or an error; by convention, the 'Left' constructor is
used to hold an error value and the 'Right' constructor is used to
hold a correct value (mnemonic: \"right\" also means \"correct\").
-}
data  Either a b  =  Left a | Right b	deriving (Eq, Ord )

-- | Case analysis for the 'Either' type.
-- If the value is @'Left' a@, apply the first function to @a@;
-- if it is @'Right' b@, apply the second function to @b@.
either                  :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x)     =  f x
either _ g (Right y)    =  g y
#endif  /* __GLASGOW_HASKELL__ */
