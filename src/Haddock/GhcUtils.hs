--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}


module Haddock.GhcUtils where


import Data.Char
import Data.Version
import qualified Data.Map as Map
import Control.Arrow
import Data.Foldable hiding (concatMap)
import Data.Traversable

import HsSyn
import SrcLoc
import Outputable
import Name
import Packages
import Module


moduleString :: Module -> String
moduleString = moduleNameString . moduleName 


-- return the name of the package, with version info
modulePackageString :: Module -> String
modulePackageString = packageIdString . modulePackageId


-- return the (name,version) of the package
modulePackageInfo :: Module -> (String, [Char])
modulePackageInfo modu = case unpackPackageId pkg of
                          Nothing -> (packageIdString pkg, "")
#if __GLASGOW_HASKELL__ >= 609
                          Just x -> (display $ pkgName x, showVersion (pkgVersion x))
#else
                          Just x -> (pkgName x, showVersion (pkgVersion x))
#endif
  where pkg = modulePackageId modu


mkModuleNoPackage :: String -> Module
mkModuleNoPackage str = mkModule (stringToPackageId "") (mkModuleName str)


instance (Outputable a, Outputable b) => Outputable (Map.Map a b) where
  ppr m = ppr (Map.toList m)


isNameSym :: Name -> Bool
isNameSym = isSymOcc . nameOccName


isVarSym :: OccName -> Bool
isVarSym = isLexVarSym . occNameFS


getMainDeclBinder :: HsDecl name -> Maybe name
getMainDeclBinder (TyClD d) = Just (tcdName d)
getMainDeclBinder (ValD d)
   = case collectAcc d [] of
        []       -> Nothing 
        (name:_) -> Just (unLoc name)
getMainDeclBinder (SigD d) = sigNameNoLoc d
getMainDeclBinder (ForD (ForeignImport name _ _)) = Just (unLoc name)
getMainDeclBinder (ForD (ForeignExport _ _ _)) = Nothing
getMainDeclBinder _ = Nothing


isTyClD :: HsDecl a -> Bool
isTyClD (TyClD _) = True
isTyClD _ = False


isClassD :: HsDecl a -> Bool
isClassD (TyClD d) = isClassDecl d
isClassD _ = False


isDocD :: HsDecl a -> Bool
isDocD (DocD _) = True
isDocD _ = False


isInstD :: HsDecl a -> Bool
isInstD (InstD _) = True
isInstD (TyClD d) = isFamInstDecl d
isInstD _ = False


declATs :: HsDecl a -> [a]
declATs (TyClD d) | isClassDecl d = map (tcdName . unL) $ tcdATs d
declATs _ = []


pretty :: Outputable a => a -> String
pretty x = showSDoc (ppr x)


trace_ppr :: Outputable a => a -> b -> b
trace_ppr x y = trace (pretty x) y


-------------------------------------------------------------------------------
-- Located
-------------------------------------------------------------------------------


unL :: Located a -> a
unL (L _ x) = x


reL :: a -> Located a
reL = L undefined


instance Foldable Located where
  foldMap f (L _ x) = f x


instance Traversable Located where
  mapM f (L l x) = (return . L l) =<< f x  


-------------------------------------------------------------------------------
-- NamedThing instances
-------------------------------------------------------------------------------


instance NamedThing (TyClDecl Name) where
  getName = tcdName


instance NamedThing (ConDecl Name) where
  getName = unL . con_name


-------------------------------------------------------------------------------
-- Subordinates
-------------------------------------------------------------------------------


class Parent a where
  children :: a -> [Name]


instance Parent (ConDecl Name) where
  children con =
    case con_details con of
      RecCon fields -> map (unL . cd_fld_name) fields
      _             -> []


instance Parent (TyClDecl Name) where
  children d
    | isDataDecl  d = map (unL . con_name . unL) . tcdCons $ d
    | isClassDecl d =
        map (tcdName . unL) (tcdATs d) ++
        [ unL n | L _ (TypeSig n _) <- tcdSigs d ]
    | otherwise = []


-- | A parent and its children
family :: (NamedThing a, Parent a) => a -> (Name, [Name])
family = getName &&& children


-- | A mapping from the parent (main-binder) to its children and from each
-- child to its grand-children, recursively.
families :: TyClDecl Name -> [(Name, [Name])]
families d
  | isDataDecl  d = family d : map (family . unL) (tcdCons d)
  | isClassDecl d = family d : concatMap (families . unL) (tcdATs d)
  | otherwise     = []


-- | A mapping from child to parent
parentMap :: TyClDecl Name -> [(Name, Name)]
parentMap d = [ (c, p) | (p, cs) <- families d, c <- cs ]


-- | The parents of a subordinate in a declaration
parents :: Name -> HsDecl Name -> [Name]
parents n (TyClD d) = [ p | (c, p) <- parentMap d, c == n ]
parents _ _ = []
