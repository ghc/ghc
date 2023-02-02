{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Dynamic
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The Dynamic interface provides basic support for dynamic types.
--
-- Operations for injecting values of arbitrary type into
-- a dynamically typed value, Dynamic, are provided, together
-- with operations for converting dynamic values into a concrete
-- (monomorphic) type.
--
-----------------------------------------------------------------------------

module Data.Dynamic
  (

        -- * The @Dynamic@ type
        Dynamic(..),

        -- * Converting to and from @Dynamic@
        toDyn,
        fromDyn,
        fromDynamic,

        -- * Applying functions of dynamic type
        dynApply,
        dynApp,
        dynTypeRep,

        -- * Convenience re-exports
        Typeable

  ) where


import Data.Type.Equality
import Type.Reflection
import Data.Maybe

import GHC.Base
import GHC.Show
import GHC.Exception

-------------------------------------------------------------
--
--              The type Dynamic
--
-------------------------------------------------------------

{-|
  A value of type 'Dynamic' is an object encapsulated together with its type.

  A 'Dynamic' may only represent a monomorphic value; an attempt to
  create a value of type 'Dynamic' from a polymorphically-typed
  expression will result in an ambiguity error (see 'toDyn').

  'Show'ing a value of type 'Dynamic' returns a pretty-printed representation
  of the object\'s type; useful for debugging.
-}
data Dynamic where
    Dynamic :: forall a. TypeRep a -> a -> Dynamic

-- | @since 2.01
instance Show Dynamic where
   -- the instance just prints the type representation.
   showsPrec _ (Dynamic t _) =
          showString "<<" .
          showsPrec 0 t   .
          showString ">>"

-- here so that it isn't an orphan:
-- | @since 4.0.0.0
instance Exception Dynamic

-- | Converts an arbitrary value into an object of type 'Dynamic'.
--
-- The type of the object must be an instance of 'Typeable', which
-- ensures that only monomorphically-typed objects may be converted to
-- 'Dynamic'.  To convert a polymorphic object into 'Dynamic', give it
-- a monomorphic type signature.  For example:
--
-- >    toDyn (id :: Int -> Int)
--
toDyn :: Typeable a => a -> Dynamic
toDyn v = Dynamic typeRep v

-- | Converts a 'Dynamic' object back into an ordinary Haskell value of
-- the correct type.  See also 'fromDynamic'.
fromDyn :: Typeable a
        => Dynamic      -- ^ the dynamically-typed object
        -> a            -- ^ a default value
        -> a            -- ^ returns: the value of the first argument, if
                        -- it has the correct type, otherwise the value of
                        -- the second argument.
fromDyn (Dynamic t v) def
  | Just HRefl <- t `eqTypeRep` typeOf def = v
  | otherwise                              = def

-- | Converts a 'Dynamic' object back into an ordinary Haskell value of
-- the correct type.  See also 'fromDyn'.
fromDynamic
        :: forall a. Typeable a
        => Dynamic      -- ^ the dynamically-typed object
        -> Maybe a      -- ^ returns: @'Just' a@, if the dynamically-typed
                        -- object has the correct type (and @a@ is its value),
                        -- or 'Nothing' otherwise.
fromDynamic (Dynamic t v)
  | Just HRefl <- t `eqTypeRep` rep = Just v
  | otherwise                       = Nothing
  where rep = typeRep :: TypeRep a

-- (f::(a->b)) `dynApply` (x::a) = (f a)::b
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply (Dynamic (Fun ta tr) f) (Dynamic ta' x)
  | Just HRefl <- ta `eqTypeRep` ta'
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind tr
  = Just (Dynamic tr (f x))
dynApply _ _
  = Nothing

dynApp :: Dynamic -> Dynamic -> Dynamic
dynApp f x = case dynApply f x of
             Just r -> r
             Nothing -> errorWithoutStackTrace ("Type error in dynamic application.\n" ++
                               "Can't apply function " ++ show f ++
                               " to argument " ++ show x)

dynTypeRep :: Dynamic -> SomeTypeRep
dynTypeRep (Dynamic tr _) = SomeTypeRep tr
