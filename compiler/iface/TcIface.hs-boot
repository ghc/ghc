module TcIface where

import GhcPrelude
import IfaceSyn    ( IfaceDecl, IfaceClsInst, IfaceFamInst, IfaceRule,
                     IfaceAnnotation, IfaceCompleteMatch )
import GHC.Core.TyCoRep     ( TyThing )
import TcRnTypes   ( IfL )
import GHC.Core.InstEnv     ( ClsInst )
import GHC.Core.FamInstEnv  ( FamInst )
import GHC.Core    ( CoreRule )
import HscTypes    ( CompleteMatch )
import Annotations ( Annotation )

tcIfaceDecl         :: Bool -> IfaceDecl -> IfL TyThing
tcIfaceRules        :: Bool -> [IfaceRule] -> IfL [CoreRule]
tcIfaceInst         :: IfaceClsInst -> IfL ClsInst
tcIfaceFamInst      :: IfaceFamInst -> IfL FamInst
tcIfaceAnnotations  :: [IfaceAnnotation] -> IfL [Annotation]
tcIfaceCompleteSigs :: [IfaceCompleteMatch] -> IfL [CompleteMatch]
