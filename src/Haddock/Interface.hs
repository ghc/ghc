--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--

-- Here we build the actual module interfaces. By interface we mean the 
-- information that is used to render a Haddock page for a module. Parts of 
-- this information is also stored in the interface files.


module Haddock.Interface (
  createInterfaces
) where


import Haddock.Interface.Create
import Haddock.Interface.AttachInstances
import Haddock.Interface.Rename
import Haddock.Types
import Haddock.Options
import Haddock.GHC.Utils

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Control.Monad.Writer
import Control.Monad

import Name


-- | Turn a topologically sorted list of GhcModules into interfaces. Also
-- return the home link environment created in the process, and any error
-- messages.
createInterfaces :: [GhcModule] -> LinkEnv -> [Flag] -> ([Interface], LinkEnv, [ErrMsg])
createInterfaces modules extLinks flags = (interfaces, homeLinks, messages)
  where 
    ((interfaces, homeLinks), messages) = runWriter $ do
      -- part 1, create the interfaces
      interfaces <- createInterfaces' modules flags
      -- part 2, attach the instances
      let interfaces' = attachInstances interfaces
      -- part 3, rename the interfaces
      renameInterfaces interfaces' extLinks


createInterfaces' :: [GhcModule] -> [Flag] -> ErrMsgM [Interface]
createInterfaces' modules flags = do
  resultMap <- foldM addInterface Map.empty modules
  return (Map.elems resultMap)
  where
    addInterface :: ModuleMap -> GhcModule -> ErrMsgM ModuleMap
    addInterface map mod = do
      interface <- createInterface mod flags map
      return $ Map.insert (hmod_mod interface) interface map

 
renameInterfaces :: [Interface] -> LinkEnv -> ErrMsgM ([Interface], LinkEnv)
renameInterfaces interfaces externalLinks = do
  let homeLinks = buildHomeLinks interfaces
  let links = homeLinks `Map.union` externalLinks
  interfaces' <- mapM (renameInterface links) interfaces
  return (interfaces', homeLinks)


-- | Build a mapping which for each original name, points to the "best"
-- place to link to in the documentation.  For the definition of
-- "best", we use "the module nearest the bottom of the dependency
-- graph which exports this name", not including hidden modules.  When
-- there are multiple choices, we pick a random one.
-- 
-- The interfaces are passed in in topologically sorted order, but we start
-- by reversing the list so we can do a foldl.
buildHomeLinks :: [Interface] -> LinkEnv
buildHomeLinks modules = foldl upd Map.empty (reverse modules)
  where
    upd old_env mod
      | OptHide    `elem` hmod_options mod = old_env
      | OptNotHome `elem` hmod_options mod =
        foldl' keep_old old_env exported_names
      | otherwise = foldl' keep_new old_env exported_names
      where
        exported_names = hmod_visible_exports mod
        modName = hmod_mod mod

        keep_old env n = Map.insertWith (\new old -> old) n
                         (nameSetMod n modName) env
        keep_new env n = Map.insert n (nameSetMod n modName) env
