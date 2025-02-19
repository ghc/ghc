module GHC.IfaceToCore where

import GHC.Prelude
import GHC.Iface.Syntax ( IfaceDecl, IfaceDefault, IfaceClsInst, IfaceFamInst, IfaceRule
                        , IfaceAnnotation, IfaceCompleteMatch )
import GHC.Types.TyThing   ( TyThing )
import GHC.Tc.Types        ( IfG, IfL )
import GHC.Core.InstEnv    ( ClsInst )
import GHC.Core.FamInstEnv ( FamInst )
import GHC.Core         ( CoreRule )
import GHC.Types.CompleteMatch
import GHC.Types.Annotations ( Annotation )
import GHC.Types.DefaultEnv ( ClassDefaults )
import GHC.Types.Name
import GHC.Unit.Types      ( Module )
import GHC.Fingerprint.Type

import Data.List.NonEmpty ( NonEmpty )
import GHC.Unit.Module.ModIface ( ModIface )
import GHC.Unit.Module.ModDetails ( ModDetails )

tcIfaceDecl            :: Bool -> IfaceDecl -> IfL TyThing
tcIfaceRules           :: Bool -> [IfaceRule] -> IfL [CoreRule]
tcIfaceDefaults        :: Module -> [(Module, IfaceDefault)] -> IfG [NonEmpty ClassDefaults]
tcIfaceInst            :: IfaceClsInst -> IfL ClsInst
tcIfaceFamInst         :: IfaceFamInst -> IfL FamInst
tcIfaceAnnotations     :: [IfaceAnnotation] -> IfL [Annotation]
tcIfaceCompleteMatches :: [IfaceCompleteMatch] -> IfL CompleteMatches
tcIfaceDecls           :: Bool -> [(Fingerprint, IfaceDecl)] -> IfL [(Name,TyThing)]
typecheckIface         :: ModIface -> IfG ModDetails
