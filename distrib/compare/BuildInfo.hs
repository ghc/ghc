
module BuildInfo where

import Control.Monad.State

data BuildInfo = BuildInfo {
                     biThingVersionMap :: ThingVersionMap,
                     biWays :: Ways
                 }
-- Mapping from thing (e.g. "Cabal") to version (e.g. "1.10.0.0")
type ThingVersionMap = [(String, String)]
-- The list of ways in the order the build system uses them, e.g.
-- ["v", "p", "dyn"] => we have ".depend-v-p-dyn.haskell" files
type Ways = [String]

addThingVersion :: ThingVersionMap -> String -> String -> Maybe ThingVersionMap
addThingVersion mapping thing version
 = case lookup thing mapping of
   Just version' ->
       if version == version'
       then Just mapping
       else Nothing
   Nothing ->
       Just ((thing, version) : mapping)

getThingVersionMap :: State BuildInfo ThingVersionMap
getThingVersionMap = do st <- get
                        return $ biThingVersionMap st

getWays :: State BuildInfo Ways
getWays = do st <- get
             return $ biWays st

putThingVersionMap :: ThingVersionMap -> State BuildInfo ()
putThingVersionMap tm = do st <- get
                           put $ st { biThingVersionMap = tm }

putWays :: Ways -> State BuildInfo ()
putWays ws = do st <- get
                put $ st { biWays = ws }

