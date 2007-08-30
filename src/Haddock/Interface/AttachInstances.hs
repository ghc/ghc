--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.Interface.AttachInstances (attachInstances) where


import Haddock.Types

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List

import GHC
import Name
import SrcLoc
import InstEnv
import Class
import TypeRep
import Var hiding (varName)
import TyCon
import PrelNames
import HscTypes
import FastString
#define FSLIT(x) (mkFastString# (x#))


attachInstances :: [Interface] -> [Interface]
attachInstances modules = map attach modules
  where
    instMap = fmap (map toHsInstHead . sortImage instHead) $ collectInstances modules
    attach mod = mod { ifaceExportItems = newItems }
      where
        newItems = map attachExport (ifaceExportItems mod)

        attachExport (ExportDecl n decl doc _) =
          ExportDecl n decl doc (case Map.lookup n instMap of
                                   Nothing -> []
                                   Just instheads -> instheads)
        attachExport otherExport = otherExport


-- | Simplified type for sorting types, ignoring qualification (not visible
-- in Haddock output) and unifying special tycons with normal ones.
data SimpleType = SimpleType Name [SimpleType] deriving (Eq,Ord)


collectInstances
   :: [Interface]
   -> Map Name [([TyVar], [PredType], Class, [Type])]  -- maps class/type names to instances

collectInstances modules
  = Map.fromListWith (flip (++)) tyInstPairs `Map.union`
    Map.fromListWith (flip (++)) classInstPairs
  where
    allInstances = concat (map ifaceInstances modules)
    classInstPairs = [ (is_cls inst, [instanceHead inst]) | 
                       inst <- allInstances ]
    tyInstPairs = [ (tycon, [instanceHead inst]) | inst <- allInstances, 
                    Just tycon <- nub (is_tcs inst) ]


instHead :: ([TyVar], [PredType], Class, [Type]) -> ([Int], Name, [SimpleType])
instHead (_, _, cls, args)
  = (map argCount args, className cls, map simplify args)
  where
    argCount (AppTy t _) = argCount t + 1
    argCount (TyConApp _ ts) = length ts
    argCount (FunTy _ _ ) = 2
    argCount (ForAllTy _ t) = argCount t
    argCount (NoteTy _ t) = argCount t
    argCount _ = 0

    simplify (ForAllTy _ t) = simplify t
    simplify (FunTy t1 t2) = 
      SimpleType funTyConName [simplify t1, simplify t2]
    simplify (AppTy t1 t2) = SimpleType s (args ++ [simplify t2])
      where (SimpleType s args) = simplify t1
    simplify (TyVarTy v) = SimpleType (tyVarName v) []
    simplify (TyConApp tc ts) = SimpleType (tyConName tc) (map simplify ts)
    simplify (NoteTy _ t) = simplify t
    simplify _ = error "simplify"


-- sortImage f = sortBy (\x y -> compare (f x) (f y))
sortImage :: Ord b => (a -> b) -> [a] -> [a]
sortImage f xs = map snd $ sortBy cmp_fst [(f x, x) | x <- xs]
 where cmp_fst (x,_) (y,_) = compare x y


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


toLHsType = noLoc . toHsType

 
toHsType :: Type -> HsType Name
toHsType t = case t of 
  TyVarTy v -> HsTyVar (tyVarName v) 
  AppTy a b -> HsAppTy (toLHsType a) (toLHsType b)
  TyConApp tc ts -> case ts of 
    [] -> HsTyVar (tyConName tc)
    _  -> app (tycon tc) ts
  FunTy a b -> HsFunTy (toLHsType a) (toLHsType b) 
  ForAllTy v t -> cvForAll [v] t 
  PredTy p -> HsPredTy (toHsPred p) 
  NoteTy _ t -> toHsType t
  where
    tycon tc = HsTyVar (tyConName tc)
    app tc ts = foldl (\a b -> HsAppTy (noLoc a) (noLoc b)) tc (map toHsType ts)
    cvForAll vs (ForAllTy v t) = cvForAll (v:vs) t
    cvForAll vs t = mkExplicitHsForAllTy (tyvarbinders vs) (noLoc []) (toLHsType t)
    tyvarbinders vs = map (noLoc . UserTyVar . tyVarName) vs
