{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Eliminate (inspect, inspectBare, ignoreBottom, ignoreCall, forceBottom, unpackLocal) where

import qualified GHC.Exists as Exists
import Provider

{-# NOINLINE inspect #-}
inspect :: Hidden -> String
inspect p = case p of
  Exists.Pack (type a) (x :: Maybe a) ->
    case x of
      Just _ -> "present"
      Nothing -> "absent"

{-# NOINLINE inspectBare #-}
inspectBare :: Hidden -> String
inspectBare p = case p of
  Exists.Pack a (x :: Maybe a) ->
    case x of
      Just _ -> "present"
      Nothing -> "absent"

{-# NOINLINE ignoreBottom #-}
ignoreBottom :: String
ignoreBottom = case lazyPack of Exists.Pack _ _ -> "ignored"

{-# NOINLINE ignoreCall #-}
ignoreCall :: String
ignoreCall = case error "package forced" :: Hidden of
  Exists.Pack (type a) _ -> "ignored application"

{-# NOINLINE forceBottom #-}
forceBottom :: String
forceBottom = case lazyPack of Exists.Pack (type _) (Just _) -> "present"

{-# NOINLINE unpackLocal #-}
unpackLocal :: String
unpackLocal = case Exists.Pack @Hidden Bool Nothing of
  Exists.Pack (type a) (x :: Maybe a) ->
    case x of
      Just _ -> "present"
      Nothing -> "absent"
