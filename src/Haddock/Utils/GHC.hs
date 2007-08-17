--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--

module Haddock.Utils.GHC where

import Debug.Trace

import GHC
import HsSyn
import SrcLoc
import HscTypes
import Outputable
import Packages
import UniqFM
import Name

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

-- names

nameSetMod n newMod = 
  mkExternalName (nameUnique n) newMod (nameOccName n) (nameSrcSpan n)

nameSetPkg pkgId n = 
  mkExternalName (nameUnique n) (mkModule pkgId (moduleName mod)) 
	               (nameOccName n) (nameSrcSpan n)
  where mod = nameModule n
