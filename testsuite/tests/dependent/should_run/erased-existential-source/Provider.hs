{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeApplications #-}
module Provider (Hidden, supplied, synthesized, kindOnly, kindAndSchema,
                 wildcardWitness, written, lazyPack) where

import Data.Kind (Type)
import qualified GHC.Exists as Exists

type Hidden = exists (a :: Type) | Maybe a

{-# NOINLINE supplied #-}
supplied :: Hidden
supplied = Exists.Pack (type Bool) (Just True)

{-# NOINLINE synthesized #-}
synthesized = Exists.Pack @Hidden Int (Just (42 :: Int))

{-# NOINLINE kindOnly #-}
kindOnly :: Hidden
kindOnly = Exists.Pack @Type Bool Nothing

{-# NOINLINE kindAndSchema #-}
kindAndSchema = Exists.Pack @Type @Hidden Char (Just 'a')

{-# NOINLINE wildcardWitness #-}
wildcardWitness :: Hidden
wildcardWitness = Exists.Pack _ (Just False)

{-# NOINLINE written #-}
written = Exists.Pack @(exists (a :: Type) | Maybe a) (type Char) (Just 'w')

{-# NOINLINE lazyPack #-}
lazyPack :: Hidden
lazyPack = Exists.Pack (type Bool) (error "packed payload forced")
