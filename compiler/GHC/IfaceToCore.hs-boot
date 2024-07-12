module GHC.IfaceToCore where

import GHC.Prelude
import GHC.Iface.Syntax ( IfaceDecl, IfaceClsInst, IfaceFamInst, IfaceRule
                        , IfaceAnnotation, IfaceCompleteMatch )
import GHC.Types.TyThing   ( TyThing )
import GHC.Tc.Types        ( IfL )
import GHC.Core.InstEnv    ( ClsInst )
import GHC.Core.FamInstEnv ( FamInst )
import GHC.Core         ( CoreRule )
import GHC.Types.CompleteMatch
import GHC.Types.Annotations ( Annotation )
import GHC.Types.Name
import GHC.Fingerprint.Type

tcIfaceDecl            :: Bool -> IfaceDecl -> IfL TyThing
tcIfaceRules           :: Bool -> [IfaceRule] -> IfL [CoreRule]
tcIfaceInst            :: IfaceClsInst -> IfL ClsInst
tcIfaceFamInst         :: IfaceFamInst -> IfL FamInst
tcIfaceAnnotations     :: [IfaceAnnotation] -> IfL [Annotation]
tcIfaceCompleteMatches :: [IfaceCompleteMatch] -> IfL CompleteMatches
tcIfaceDecls           :: Bool -> [(Fingerprint, IfaceDecl)] -> IfL [(Name,TyThing)]
