module TcIface where

import GhcPrelude
import IfaceSyn    ( IfaceDecl, IfaceClsInst, IfaceFamInst, IfaceRule,
                     IfaceAnnotation, IfaceCompleteMatch, IfaceExpr, IfaceType)
import TyCoRep     ( TyThing, Type )
import TcRnTypes   ( IfL )
import InstEnv     ( ClsInst )
import FamInstEnv  ( FamInst )
import CoreSyn     ( CoreRule, CoreExpr )
import HscTypes    ( CompleteMatch )
import Annotations ( Annotation )
import Name        ( Name )

tcIfaceDecl         :: Bool -> IfaceDecl -> IfL TyThing
tcIfaceRules        :: Bool -> [IfaceRule] -> IfL [CoreRule]
tcIfaceInst         :: IfaceClsInst -> IfL ClsInst
tcIfaceFamInst      :: IfaceFamInst -> IfL FamInst
tcIfaceAnnotations  :: [IfaceAnnotation] -> IfL [Annotation]
tcIfaceCompleteSigs :: [IfaceCompleteMatch] -> IfL [CompleteMatch]
tcIfaceExpr         :: IfaceExpr -> IfL CoreExpr
tcIfaceType         :: IfaceType -> IfL Type
tcIfaceGlobal       :: Name -> IfL TyThing
