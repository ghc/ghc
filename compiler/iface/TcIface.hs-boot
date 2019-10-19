module TcIface where

import GhcPrelude
import IfaceSyn    ( IfaceDecl, IfaceClsInst, IfaceFamInst, IfaceRule,
                     IfaceAnnotation, IfaceCompleteMatch, IfLFInfo )
import TyCoRep     ( TyThing )
import TcRnTypes   ( IfL )
import InstEnv     ( ClsInst )
import FamInstEnv  ( FamInst )
import CoreSyn     ( CoreRule )
import HscTypes    ( CompleteMatch )
import Annotations ( Annotation )
import GHC.StgToCmm.CgTypes     ( LambdaFormInfo )
import Name        ( Name )
import NameEnv     ( NameEnv )

tcIfaceDecl         :: Bool -> IfaceDecl -> IfL TyThing
tcIfaceRules        :: Bool -> [IfaceRule] -> IfL [CoreRule]
tcIfaceInst         :: IfaceClsInst -> IfL ClsInst
tcIfaceFamInst      :: IfaceFamInst -> IfL FamInst
tcIfaceAnnotations  :: [IfaceAnnotation] -> IfL [Annotation]
tcIfaceCompleteSigs :: [IfaceCompleteMatch] -> IfL [CompleteMatch]

tcCodeGenInfos :: NameEnv LambdaFormInfo -> [(Name,IfLFInfo)] -> IfL (NameEnv LambdaFormInfo)