{-# LANGUAGE MagicHash #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Interface.AttachInstances
-- Copyright   :  (c) Simon Marlow 2006,
--                    David Waern  2006-2009,
--                    Isaac Dupree 2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Haddock.Interface.AttachInstances (attachInstances) where


import Haddock.Types
import Haddock.Convert

import Control.Arrow
import Data.List
import qualified Data.Map as Map

import GHC
import Name
import InstEnv
import Class
import HscTypes (withSession, ioMsg)
import TcRnDriver (tcRnGetInfo)

#if __GLASGOW_HASKELL__ > 610 || (__GLASGOW_HASKELL__ == 610 && __GHC_PATCHLEVEL__ >= 2)
import TypeRep hiding (funTyConName)
#else
import TypeRep
#endif

import Var hiding (varName)
import TyCon
import PrelNames
import FastString
#define FSLIT(x) (mkFastString# (x#))


attachInstances :: [Interface] -> InstIfaceMap -> Ghc [Interface]
attachInstances ifaces instIfaceMap = mapM attach ifaces
  where
    attach iface = do
      newItems <- mapM attachExport $ ifaceExportItems iface
      return $ iface { ifaceExportItems = newItems }
      where
        attachExport export@ExportDecl{expItemDecl = L _ (TyClD d)} = do
           mb_info <- getAllInfo (unLoc (tcdLName d))
           return $ export { expItemInstances = case mb_info of
             Just (_, _, instances) ->
               let insts = map (first synifyInstHead) $ sortImage (first instHead)
                             [ (instanceHead i, getName i) | i <- instances ]
               in [ (inst, lookupInstDoc name iface instIfaceMap)
                  | (inst, name) <- insts ]
             Nothing -> []
            }
        attachExport export = return export


lookupInstDoc :: Name -> Interface -> InstIfaceMap -> Maybe (HsDoc Name)
-- TODO: capture this pattern in a function (when we have streamlined the
-- handling of instances)
lookupInstDoc name iface ifaceMap =
  case Map.lookup name (ifaceInstanceDocMap iface) of
    Just doc -> Just doc
    Nothing -> do -- in Maybe
      instIface <- Map.lookup modName ifaceMap
      (Just doc, _) <- Map.lookup name (instDocMap instIface)
      return doc
  where
    modName = nameModule name


-- | Like GHC's getInfo but doesn't cut things out depending on the
-- interative context, which we don't set sufficiently anyway.
getAllInfo :: GhcMonad m => Name -> m (Maybe (TyThing,Fixity,[Instance]))
getAllInfo name = withSession $ \hsc_env -> ioMsg $ tcRnGetInfo hsc_env name


--------------------------------------------------------------------------------
-- Collecting and sorting instances
--------------------------------------------------------------------------------


-- | Simplified type for sorting types, ignoring qualification (not visible
-- in Haddock output) and unifying special tycons with normal ones.
-- For the benefit of the user (looks nice and predictable) and the
-- tests (which prefer output to be deterministic).
data SimpleType = SimpleType Name [SimpleType] deriving (Eq,Ord)


-- TODO: should we support PredTy here?
instHead :: ([TyVar], [PredType], Class, [Type]) -> ([Int], Name, [SimpleType])
instHead (_, _, cls, args)
  = (map argCount args, className cls, map simplify args)
  where
    argCount (AppTy t _) = argCount t + 1
    argCount (TyConApp _ ts) = length ts
    argCount (FunTy _ _ ) = 2
    argCount (ForAllTy _ t) = argCount t
    argCount _ = 0

    simplify (ForAllTy _ t) = simplify t
    simplify (FunTy t1 t2) = 
      SimpleType funTyConName [simplify t1, simplify t2]
    simplify (AppTy t1 t2) = SimpleType s (ts ++ [simplify t2])
      where (SimpleType s ts) = simplify t1
    simplify (TyVarTy v) = SimpleType (tyVarName v) []
    simplify (TyConApp tc ts) = SimpleType (tyConName tc) (map simplify ts)
    simplify _ = error "simplify"


-- sortImage f = sortBy (\x y -> compare (f x) (f y))
sortImage :: Ord b => (a -> b) -> [a] -> [a]
sortImage f xs = map snd $ sortBy cmp_fst [(f x, x) | x <- xs]
 where cmp_fst (x,_) (y,_) = compare x y


funTyConName :: Name
funTyConName = mkWiredInName gHC_PRIM
                        (mkOccNameFS tcName FSLIT("(->)"))
                        funTyConKey
                        (ATyCon funTyCon)       -- Relevant TyCon
                        BuiltInSyntax
