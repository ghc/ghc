{-# LANGUAGE ExperimentalDependentTypes, GADTs #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
module Eliminate (inspect, inspectBare, ignoreBottom, ignoreCall, forceBottom, unpackLocal, shown, render, inlineArgument, transportPair, hidePair, readPair) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import qualified GHC.Exists as Exists
import Provider
import OrdinaryPack (observeArgument)

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


-- A list dictionary used by the specialiser depends on the opened witness.
data Shown a where
  Shown :: Show a => a -> Shown a

type HiddenShow = exists (a :: Type) | Shown a

{-# NOINLINE shown #-}
shown :: forall a. Show a => a -> HiddenShow
shown x = Exists.Pack (type a) (Shown x)

{-# INLINABLE renderList #-}
renderList :: Show a => a -> String
renderList = show

{-# NOINLINE render #-}
render :: HiddenShow -> String
render p = case p of
  Exists.Pack (type a) (Shown x) -> renderList [x :: a]

-- Exercise argument classification at an imported, polymorphic call.
inlineArgument :: Bool -> Int -> Int
inlineArgument x choice = observeArgument (Just x :: exists (a :: Type). Maybe a) choice

type HiddenPair a = exists (w :: Type) | (w, a)

hidePair :: a -> HiddenPair a
hidePair x = Exists.Pack (type Bool) (True, x)

-- Lift the given equality under the same erased witness on both sides.
{-# NOINLINE transportPair #-}
transportPair :: a :~: b -> HiddenPair a -> HiddenPair b
transportPair Refl p = p

readPair :: HiddenPair a -> a
readPair p = case p of Exists.Pack _ pair -> snd pair
