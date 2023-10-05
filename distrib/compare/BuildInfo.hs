
module BuildInfo where

import Control.Monad.State

type BIMonad = StateT BuildInfo Maybe

data BuildInfo = BuildInfo {
                     biThingVersionMap :: ThingVersionMap,
                     biThingHashMap :: ThingHashMap,
                     biMaybeWays :: Maybe Ways
                 }
    deriving Show

type ThingMap = [(String, String)]
-- Mapping from thing (e.g. "Cabal") to version (e.g. "1.10.0.0")
type ThingVersionMap = ThingMap
-- Mapping from thing (e.g. "Cabal") to ABI hash
-- (e.g. "e1f7c380581d61d42b0360d440cc35ed")
type ThingHashMap = ThingMap
-- The list of ways in the order the build system uses them, e.g.
-- ["v", "p", "dyn"] => we have ".depend-v-p-dyn.haskell" files
type Ways = [String]

emptyBuildInfo :: Maybe Ways -> BuildInfo
emptyBuildInfo mWays = BuildInfo {
                           biThingVersionMap = [],
                           biThingHashMap = [],
                           biMaybeWays = mWays
                       }

addThingMap :: ThingMap -> String -> String -> Maybe ThingMap
addThingMap mapping thing str
 = case lookup thing mapping of
   Just str' ->
       if str == str'
       then Just mapping
       else Nothing
   Nothing ->
       Just ((thing, str) : mapping)

getMaybeWays :: BIMonad (Maybe Ways)
getMaybeWays = do st <- get
                  return $ biMaybeWays st

haveThingVersion :: String -> String -> BIMonad ()
haveThingVersion thing thingVersion
 = do st <- get
      case addThingMap (biThingVersionMap st) thing thingVersion of
          Nothing  -> fail "Inconsistent version"
          Just tvm -> put $ st { biThingVersionMap = tvm }

haveThingHash :: String -> String -> BIMonad ()
haveThingHash thing thingHash
 = do st <- get
      case addThingMap (biThingHashMap st) thing thingHash of
          Nothing  -> fail "Inconsistent hash"
          Just thm -> put $ st { biThingHashMap = thm }

