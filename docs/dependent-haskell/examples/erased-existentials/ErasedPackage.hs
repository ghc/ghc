{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# OPTIONS_GHC -O0 #-}

-- | Observe payloads whose type witness is erased.
module ErasedPackage (observation) where

import Data.Kind (Type)
import GHC.Exists

type Hidden = exists (a :: Type) | Maybe a

present :: Hidden -> Bool
present package = case package of
  Pack (type a) payload ->
    case payload of
      Nothing -> False
      Just _ -> True

-- | Open two defined packages and ignore an exceptional package.
observation :: (Bool, Bool, Bool)
observation =
  ( present (Pack (type Bool) (Just True))
  , present (Pack (type Char) Nothing)
  , case error "unused package" :: Hidden of
      Pack (type a) _ -> True
  )
