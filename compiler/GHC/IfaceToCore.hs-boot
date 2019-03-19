module GHC.IfaceToCore where

import GhcPrelude
import GHC.Iface.Syntax ( IfaceDecl, IfaceClsInst, IfaceFamInst, IfaceRule
                        , IfaceAnnotation, IfaceCompleteMatch, IfaceExpr, IfaceType )
import GHC.Core.TyCo.Rep   ( TyThing, Type )
import GHC.Tc.Types ( IfL )
import GHC.Core.InstEnv    ( ClsInst )
import GHC.Core.FamInstEnv ( FamInst )
import GHC.Core         ( CoreRule, CoreExpr )
import GHC.Driver.Types ( CompleteMatch )
import GHC.Types.Annotations ( Annotation )
import GHC.Types.Name (Name)

tcIfaceDecl         :: Bool -> IfaceDecl -> IfL TyThing
tcIfaceRules        :: Bool -> [IfaceRule] -> IfL [CoreRule]
tcIfaceInst         :: IfaceClsInst -> IfL ClsInst
tcIfaceFamInst      :: IfaceFamInst -> IfL FamInst
tcIfaceAnnotations  :: [IfaceAnnotation] -> IfL [Annotation]
tcIfaceCompleteSigs :: [IfaceCompleteMatch] -> IfL [CompleteMatch]
tcIfaceGlobal :: Name -> IfL TyThing
tcIfaceExpr :: IfaceExpr -> IfL CoreExpr
tcIfaceType :: IfaceType -> IfL Type
