{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , OverlappingInstances
           , ScopedTypeVariables
           , ForeignFunctionInterface
           , FlexibleInstances
           , PolyKinds
  #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- The -XOverlappingInstances flag allows the user to over-ride
-- the instances for Typeable given here.  In particular, we provide an instance
--      instance ... => Typeable (s a) 
-- But a user might want to say
--      instance ... => Typeable (MyType a b)

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Typeable
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The 'Typeable' class reifies types to some extent by associating type
-- representations to types. These type representations can be compared,
-- and one can in turn define a type-safe cast operation. To this end,
-- an unsafe cast is guarded by a test for type (representation)
-- equivalence. The module "Data.Dynamic" uses Typeable for an
-- implementation of dynamics. The module "Data.Data" uses Typeable
-- and type-safe cast (but not dynamics) to support the \"Scrap your
-- boilerplate\" style of generic programming.
--
-----------------------------------------------------------------------------

module Data.Typeable
  (
        -- * A proxy type
        Proxy (..),

        -- * The Typeable class
        Typeable( typeRep ),

        -- * For backwards compatibility
        typeOf, typeOf1, typeOf2, typeOf3, typeOf4, typeOf5, typeOf6, typeOf7,

        -- * Type-safe cast
        cast,
        gcast,                  -- a generalisation of cast

        -- * Generalized casts for higher-order kinds
        gcast1,                 -- :: ... => c (t a) -> Maybe (c (t' a))
        gcast2,                 -- :: ... => c (t a b) -> Maybe (c (t' a b))

        -- * Type representations
        TypeRep,        -- abstract, instance of: Eq, Show, Typeable
        showsTypeRep,

        TyCon,          -- abstract, instance of: Eq, Show, Typeable
        tyConString,
        tyConPackage,
        tyConModule,
        tyConName,

        -- * Construction of type representations
        -- mkTyCon,        -- :: String  -> TyCon
        mkTyCon3,       -- :: String  -> String -> String -> TyCon
        mkTyConApp,     -- :: TyCon   -> [TypeRep] -> TypeRep
        mkAppTy,        -- :: TypeRep -> TypeRep   -> TypeRep
        mkFunTy,        -- :: TypeRep -> TypeRep   -> TypeRep

        -- * Observation of type representations
        splitTyConApp,  -- :: TypeRep -> (TyCon, [TypeRep])
        funResultTy,    -- :: TypeRep -> TypeRep   -> Maybe TypeRep
        typeRepTyCon,   -- :: TypeRep -> TyCon
        typeRepArgs,    -- :: TypeRep -> [TypeRep]
  ) where

import Data.Typeable.Internal hiding (mkTyCon)

import Unsafe.Coerce
import Data.Maybe

import GHC.Base
import GHC.Err          (undefined)

-------------------------------------------------------------
--
--              Type-safe cast
--
-------------------------------------------------------------

-- | The type-safe cast operation
cast :: forall a b. (Typeable a, Typeable b) => a -> Maybe b
cast x = if typeRep (Proxy :: Proxy a) == typeRep (Proxy :: Proxy b)
           then Just $ unsafeCoerce x
           else Nothing

-- | A flexible variation parameterised in a type constructor
gcast :: (Typeable (a :: *), Typeable b) => c a -> Maybe (c b)
gcast x = r
 where
  r = if typeRep (getArg x) == typeRep (getArg (fromJust r))
        then Just $ unsafeCoerce x
        else Nothing
  getArg :: c x -> Proxy x 
  getArg = undefined

-- | Cast for * -> *
gcast1 :: forall c t t' a. (Typeable (t :: * -> *), Typeable t')
       => c (t a) -> Maybe (c (t' a)) 
gcast1 x = if typeRep (Proxy :: Proxy t) == typeRep (Proxy :: Proxy t')
             then Just $ unsafeCoerce x
             else Nothing

-- | Cast for * -> * -> *
gcast2 :: forall c t t' a b. (Typeable (t :: * -> * -> *), Typeable t')
       => c (t a b) -> Maybe (c (t' a b)) 
gcast2 x = if typeRep (Proxy :: Proxy t) == typeRep (Proxy :: Proxy t')
             then Just $ unsafeCoerce x
             else Nothing
