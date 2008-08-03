--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}


module Haddock.GHC.Utils where


import Data.Char
import Data.Version
import qualified Data.Map as Map

import GHC
import HsSyn
import SrcLoc
import Outputable
import Name
import Packages


unL :: Located a -> a
unL (L _ x) = x


reL :: a -> Located a
reL = L undefined


moduleString :: Module -> String
moduleString = moduleNameString . moduleName 


-- return the name of the package, with version info
modulePackageString = packageIdString . modulePackageId


-- return the (name,version) of the package
modulePackageInfo mod = case unpackPackageId pkg of
                          Nothing -> (packageIdString pkg, "")
                          Just x -> (pkgName x, showVersion (pkgVersion x))
    where pkg = modulePackageId mod


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


isTyClD (TyClD _) = True
isTyClD _ = False


isClassD (TyClD d) = isClassDecl d
isClassD _ = False


isDocD (DocD _) = True
isDocD _ = False


isInstD (InstD _) = True
isInstD (TyClD d) = isFamInstDecl d
isInstD _ = False


declATs (TyClD d) | isClassDecl d = map (tcdName . unL) $ tcdATs d
declATs _ = []


pretty :: Outputable a => a -> String
pretty x = showSDoc (ppr x)


trace_ppr :: Outputable a => a -> b -> b
trace_ppr x y = trace (pretty x) y
