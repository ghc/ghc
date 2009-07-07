{-# LANGUAGE MagicHash #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Interface.AttachInstances
-- Copyright   :  (c) David Waern  2006-2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Haddock.Interface.AttachInstances (attachInstances) where


import Haddock.Types

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List

import GHC
import Name
import InstEnv
import Class

#if __GLASGOW_HASKELL__ >= 610 && __GHC_PATCHLEVEL__ >= 2
import TypeRep hiding (funTyConName)
#else
import TypeRep
#endif

import Var hiding (varName)
import TyCon
import PrelNames
import FastString
#define FSLIT(x) (mkFastString# (x#))


attachInstances :: [Interface] -> [Name] -> [Interface]
attachInstances ifaces filterNames = map attach ifaces
  where
    instMap =
      fmap (map toHsInstHead . sortImage instHead) $
      collectInstances ifaces filterNames

    attach iface = iface { ifaceExportItems = newItems }
      where
        newItems = map attachExport (ifaceExportItems iface)

        attachExport (ExportDecl decl@(L _ (TyClD d)) doc subs _)
          | isClassDecl d || isDataDecl d || isFamilyDecl d =
             ExportDecl decl doc subs (case Map.lookup (tcdName d) instMap of
                                    Nothing -> []
                                    Just instheads -> instheads)
        attachExport export = export


--------------------------------------------------------------------------------
-- Collecting and sorting instances
--------------------------------------------------------------------------------


-- | Simplified type for sorting types, ignoring qualification (not visible
-- in Haddock output) and unifying special tycons with normal ones.
data SimpleType = SimpleType Name [SimpleType] deriving (Eq,Ord)


collectInstances
   :: [Interface]
   -> [Name]
   -> Map Name [([TyVar], [PredType], Class, [Type])]  -- maps class/type names to instances

collectInstances ifaces _ -- filterNames
  = Map.fromListWith (flip (++)) tyInstPairs `Map.union`
    Map.fromListWith (flip (++)) classInstPairs
  where
    allInstances = concatMap ifaceInstances ifaces
    classInstPairs = [ (is_cls inst, [instanceHead inst]) | 
                       inst <- allInstances ]
                    -- unfinished filtering of internal instances
                    -- Just n <- nub (is_tcs inst) ]
                    --   n `elem` filterNames ]
    tyInstPairs = [ (tycon, [instanceHead inst]) | inst <- allInstances, 
                    Just tycon <- nub (is_tcs inst) ]    


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


toHsInstHead :: ([TyVar], [PredType], Class, [Type]) -> InstHead Name
toHsInstHead (_, preds, cls, ts) = (map toHsPred preds, className cls, map toHsType ts) 


--------------------------------------------------------------------------------
-- Type -> HsType conversion
--------------------------------------------------------------------------------


toHsPred :: PredType -> HsPred Name
toHsPred (ClassP cls ts) = HsClassP (className cls) (map toLHsType ts)
toHsPred (IParam n t) = HsIParam n (toLHsType t)
toHsPred (EqPred t1 t2) = HsEqualP (toLHsType t1) (toLHsType t2)


toLHsType :: Type -> Located (HsType Name)
toLHsType = noLoc . toHsType

 
toHsType :: Type -> HsType Name
toHsType t = case t of 
  TyVarTy v -> HsTyVar (tyVarName v) 
  AppTy a b -> HsAppTy (toLHsType a) (toLHsType b)

  TyConApp tc ts -> case ts of 
    t1:t2:rest
      | isSymOcc . nameOccName . tyConName $ tc ->
          app (HsOpTy (toLHsType t1) (noLoc . tyConName $ tc) (toLHsType t2)) rest
    _ -> app (tycon tc) ts

  FunTy a b -> HsFunTy (toLHsType a) (toLHsType b)
  ForAllTy v ty -> cvForAll [v] ty 
  PredTy p -> HsPredTy (toHsPred p) 
  where
    tycon = HsTyVar . tyConName
    app tc = foldl (\a b -> HsAppTy (noLoc a) (noLoc b)) tc . map toHsType
    cvForAll vs (ForAllTy v ty) = cvForAll (v:vs) ty
    cvForAll vs ty = mkExplicitHsForAllTy (tyvarbinders vs) (noLoc []) (toLHsType ty)
    tyvarbinders = map (noLoc . UserTyVar . tyVarName)
