module GHCUtils where

import Debug.Trace

import GHC
import HsSyn
import SrcLoc
import HscTypes
import Outputable

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

modInfoName = moduleName . mi_module . minf_iface
modInfoMod  = mi_module . minf_iface 

trace_ppr x y = trace (showSDoc (ppr x)) y
