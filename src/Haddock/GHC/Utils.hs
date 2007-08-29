--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.GHC.Utils where


import Debug.Trace
import Data.Char

import GHC
import HsSyn
import SrcLoc
import HscTypes
import Outputable
import Packages
import UniqFM
import Name


-- names

nameOccString = occNameString . nameOccName 


nameSetMod n newMod = 
  mkExternalName (nameUnique n) newMod (nameOccName n) (nameSrcSpan n)


nameSetPkg pkgId n = 
  mkExternalName (nameUnique n) (mkModule pkgId (moduleName mod)) 
	               (nameOccName n) (nameSrcSpan n)
  where mod = nameModule n


-- modules


moduleString :: Module -> String
moduleString = moduleNameString . moduleName 


mkModuleNoPkg :: String -> Module
mkModuleNoPkg str = mkModule (stringToPackageId "") (mkModuleName str)


modulePkgStr = packageIdString . modulePackageId


-- misc


-- there should be a better way to check this using the GHC API
isConSym n = head (nameOccString n) == ':'
isVarSym n = fstChar /= '_' && not (isConSym n) && (not . isLetter) fstChar
  where fstChar = head (nameOccString n)


getMainDeclBinder :: HsDecl name -> Maybe name
getMainDeclBinder (TyClD d) = Just (tcdName d)
getMainDeclBinder (ValD d)
   = case collectAcc d [] of
        []       -> Nothing 
        (name:_) -> Just (unLoc name)
getMainDeclBinder (SigD d) = sigNameNoLoc d
getMainDeclBinder (ForD (ForeignImport name _ _)) = Just (unLoc name)
getMainDeclBinder (ForD (ForeignExport name _ _)) = Nothing
getMainDeclBinder _ = Nothing


-- To keep if if minf_iface is re-introduced
--modInfoName = moduleName . mi_module . minf_iface
--modInfoMod  = mi_module . minf_iface 


trace_ppr x y = trace (showSDoc (ppr x)) y
