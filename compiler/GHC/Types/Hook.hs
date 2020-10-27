{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Hooks can be used by GHC API clients to replace parts of the compiler
-- pipeline. If a hook is not installed, GHC uses the default built-in behaviour
module GHC.Types.Hook
   ( Hook (..)
   , IsHook (..)
   , Hooks
   , emptyHooks
   , getHook
   , setHook
   , removeHook
   )
where

import GHC.Prelude

import Data.Dynamic

-- | Hooks are stored as Dynamic values so that we can store them in a global
-- list without forcing all parts of the compiler that use hooks to depend on each other
newtype Hook = Hook Dynamic

class Typeable h => IsHook h where
   toHook :: h -> Hook
   toHook h = Hook (toDyn h)

   fromHook :: Hook -> Maybe h
   fromHook (Hook h) = fromDynamic h

-- | A list of hooks
newtype Hooks = Hooks [Hook]

emptyHooks :: Hooks
emptyHooks = Hooks []

-- | Get hook of the given type
getHook :: forall h. IsHook h => Hooks -> Maybe h
getHook (Hooks hs) = go hs
   where
      go [] = Nothing
      go (x:xs)
         | Just h <- fromHook @h x
         = Just h
         | otherwise
         = go xs

-- | Remove hook of the given type
removeHook :: forall h. IsHook h => Hooks -> Hooks
removeHook (Hooks hs) = go [] hs
   where
      go rs [] = Hooks rs
      go rs (x:xs)
         -- remove hook with type "h"
         | Just _ <- fromHook @h x
         = Hooks (rs ++ xs)
         | otherwise
         = go (x:rs) xs

-- | Set hook of the given type
setHook :: forall h. IsHook h => Hooks -> h -> Hooks
setHook hooks h = Hooks (toHook h:hs)
   where
      Hooks hs = removeHook @h hooks
