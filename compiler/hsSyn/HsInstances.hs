{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HsInstances where

-- This module defines the Data instances for the hsSyn AST.

-- It happens here to avoid massive constraint types on the AST with concomitant
-- slow GHC bootstrap times.

-- UndecidableInstances ?

import Data.Data hiding ( Fixity )

import HsExtension
import HsBinds
import HsDecls
import HsExpr
import HsLit
import HsTypes
import HsPat
import HsImpExp
import Name
import RdrName
import Id

-- Data derivations from HsBinds ---------------------------------------
deriving instance Data (HsLocalBindsLR GhcPs GhcPs)
deriving instance Data (HsLocalBindsLR GhcPs GhcRn)
deriving instance Data (HsLocalBindsLR GhcRn GhcRn)
deriving instance Data (HsLocalBindsLR GhcTc GhcTc)

deriving instance Data (HsValBindsLR GhcPs GhcPs)
deriving instance Data (HsValBindsLR GhcPs GhcRn)
deriving instance Data (HsValBindsLR GhcRn GhcRn)
deriving instance Data (HsValBindsLR GhcTc GhcTc)

deriving instance Data (NHsValBindsLR GhcPs)
deriving instance Data (NHsValBindsLR GhcRn)
deriving instance Data (NHsValBindsLR GhcTc)

deriving instance Data (HsBindLR GhcPs GhcPs)
deriving instance Data (HsBindLR GhcPs GhcRn)
deriving instance Data (HsBindLR GhcRn GhcRn)
deriving instance Data (HsBindLR GhcTc GhcTc)

deriving instance Data (ABExport GhcPs)
deriving instance Data (ABExport GhcRn)
deriving instance Data (ABExport GhcTc)

deriving instance Data (PatSynBind GhcPs GhcPs)
deriving instance Data (PatSynBind GhcPs GhcRn)
deriving instance Data (PatSynBind GhcRn GhcRn)
deriving instance Data (PatSynBind GhcTc GhcTc)

deriving instance Data (HsIPBinds GhcPs)
deriving instance Data (HsIPBinds GhcRn)
deriving instance Data (HsIPBinds GhcTc)

deriving instance Data (IPBind GhcPs)
deriving instance Data (IPBind GhcRn)
deriving instance Data (IPBind GhcTc)

deriving instance Data (Sig GhcPs)
deriving instance Data (Sig GhcRn)
deriving instance Data (Sig GhcTc)

deriving instance Data (FixitySig GhcPs)
deriving instance Data (FixitySig GhcRn)
deriving instance Data (FixitySig GhcTc)

deriving instance Data (HsPatSynDir GhcPs)
deriving instance Data (HsPatSynDir GhcRn)
deriving instance Data (HsPatSynDir GhcTc)


-- Data derivations from HsDecls ---------------------------------------

deriving instance Data (HsDecl GhcPs)
deriving instance Data (HsDecl GhcRn)
deriving instance Data (HsDecl GhcTc)

deriving instance Data (HsGroup GhcPs)
deriving instance Data (HsGroup GhcRn)
deriving instance Data (HsGroup GhcTc)

deriving instance Data (SpliceDecl GhcPs)
deriving instance Data (SpliceDecl GhcRn)
deriving instance Data (SpliceDecl GhcTc)

deriving instance Data (TyClDecl GhcPs)
deriving instance Data (TyClDecl GhcRn)
deriving instance Data (TyClDecl GhcTc)

deriving instance Data (TyClGroup GhcPs)
deriving instance Data (TyClGroup GhcRn)
deriving instance Data (TyClGroup GhcTc)

deriving instance Data (FamilyResultSig GhcPs)
deriving instance Data (FamilyResultSig GhcRn)
deriving instance Data (FamilyResultSig GhcTc)

deriving instance Data (FamilyDecl GhcPs)
deriving instance Data (FamilyDecl GhcRn)
deriving instance Data (FamilyDecl GhcTc)

deriving instance Data (InjectivityAnn GhcPs)
deriving instance Data (InjectivityAnn GhcRn)
deriving instance Data (InjectivityAnn GhcTc)

deriving instance Data (FamilyInfo GhcPs)
deriving instance Data (FamilyInfo GhcRn)
deriving instance Data (FamilyInfo GhcTc)

deriving instance Data (HsDataDefn GhcPs)
deriving instance Data (HsDataDefn GhcRn)
deriving instance Data (HsDataDefn GhcTc)

deriving instance Data (HsDerivingClause GhcPs)
deriving instance Data (HsDerivingClause GhcRn)
deriving instance Data (HsDerivingClause GhcTc)

deriving instance Data (ConDecl GhcPs)
deriving instance Data (ConDecl GhcRn)
deriving instance Data (ConDecl GhcTc)

deriving instance Data (TyFamInstDecl GhcPs)
deriving instance Data (TyFamInstDecl GhcRn)
deriving instance Data (TyFamInstDecl GhcTc)

deriving instance Data (DataFamInstDecl GhcPs)
deriving instance Data (DataFamInstDecl GhcRn)
deriving instance Data (DataFamInstDecl GhcTc)

deriving instance (Data pats, Data rhs) => Data (FamEqn GhcPs pats rhs)
deriving instance (Data pats, Data rhs) => Data (FamEqn GhcRn pats rhs)
deriving instance (Data pats, Data rhs) => Data (FamEqn GhcTc pats rhs)

deriving instance Data (ClsInstDecl GhcPs)
deriving instance Data (ClsInstDecl GhcRn)
deriving instance Data (ClsInstDecl GhcTc)

deriving instance Data (InstDecl GhcPs)
deriving instance Data (InstDecl GhcRn)
deriving instance Data (InstDecl GhcTc)

deriving instance Data (DerivDecl GhcPs)
deriving instance Data (DerivDecl GhcRn)
deriving instance Data (DerivDecl GhcTc)

deriving instance Data (DefaultDecl GhcPs)
deriving instance Data (DefaultDecl GhcRn)
deriving instance Data (DefaultDecl GhcTc)

deriving instance Data (ForeignDecl GhcPs)
deriving instance Data (ForeignDecl GhcRn)
deriving instance Data (ForeignDecl GhcTc)

deriving instance Data (RuleDecls GhcPs)
deriving instance Data (RuleDecls GhcRn)
deriving instance Data (RuleDecls GhcTc)

deriving instance Data (RuleDecl GhcPs)
deriving instance Data (RuleDecl GhcRn)
deriving instance Data (RuleDecl GhcTc)

deriving instance Data (RuleBndr GhcPs)
deriving instance Data (RuleBndr GhcRn)
deriving instance Data (RuleBndr GhcTc)

deriving instance Data (VectDecl GhcPs)
deriving instance Data (VectDecl GhcRn)
deriving instance Data (VectDecl GhcTc)

deriving instance Data (WarnDecls GhcPs)
deriving instance Data (WarnDecls GhcRn)
deriving instance Data (WarnDecls GhcTc)

deriving instance Data (WarnDecl GhcPs)
deriving instance Data (WarnDecl GhcRn)
deriving instance Data (WarnDecl GhcTc)

deriving instance Data (AnnDecl GhcPs)
deriving instance Data (AnnDecl GhcRn)
deriving instance Data (AnnDecl GhcTc)

deriving instance Data (RoleAnnotDecl GhcPs)
deriving instance Data (RoleAnnotDecl GhcRn)
deriving instance Data (RoleAnnotDecl GhcTc)

-- Data derivations from HsExpr ----------------------------------------

deriving instance Data (SyntaxExpr GhcPs)
deriving instance Data (SyntaxExpr GhcRn)
deriving instance Data (SyntaxExpr GhcTc)

deriving instance Data (HsExpr GhcPs)
deriving instance Data (HsExpr GhcRn)
deriving instance Data (HsExpr GhcTc)

deriving instance Data (HsTupArg GhcPs)
deriving instance Data (HsTupArg GhcRn)
deriving instance Data (HsTupArg GhcTc)

deriving instance Data (HsCmd GhcPs)
deriving instance Data (HsCmd GhcRn)
deriving instance Data (HsCmd GhcTc)

deriving instance Data (HsCmdTop GhcPs)
deriving instance Data (HsCmdTop GhcRn)
deriving instance Data (HsCmdTop GhcTc)

deriving instance (Data body) => Data (MatchGroup GhcPs body)
deriving instance (Data body) => Data (MatchGroup GhcRn body)
deriving instance (Data body) => Data (MatchGroup GhcTc body)

deriving instance (Data body) => Data (Match      GhcPs body)
deriving instance (Data body) => Data (Match      GhcRn body)
deriving instance (Data body) => Data (Match      GhcTc body)

deriving instance (Data body) => Data (GRHSs      GhcPs body)
deriving instance (Data body) => Data (GRHSs      GhcRn body)
deriving instance (Data body) => Data (GRHSs      GhcTc body)

deriving instance (Data body) => Data (GRHS       GhcPs body)
deriving instance (Data body) => Data (GRHS       GhcRn body)
deriving instance (Data body) => Data (GRHS       GhcTc body)

deriving instance (Data body) => Data (StmtLR   GhcPs GhcPs body)
deriving instance (Data body) => Data (StmtLR   GhcPs GhcRn body)
deriving instance (Data body) => Data (StmtLR   GhcRn GhcRn body)
deriving instance (Data body) => Data (StmtLR   GhcTc GhcTc body)

deriving instance Data (ParStmtBlock GhcPs GhcPs)
deriving instance Data (ParStmtBlock GhcPs GhcRn)
deriving instance Data (ParStmtBlock GhcRn GhcRn)
deriving instance Data (ParStmtBlock GhcTc GhcTc)

deriving instance Data (ApplicativeArg GhcPs)
deriving instance Data (ApplicativeArg GhcRn)
deriving instance Data (ApplicativeArg GhcTc)

deriving instance Data (HsSplice GhcPs)
deriving instance Data (HsSplice GhcRn)
deriving instance Data (HsSplice GhcTc)

deriving instance Data (HsSplicedThing GhcPs)
deriving instance Data (HsSplicedThing GhcRn)
deriving instance Data (HsSplicedThing GhcTc)

deriving instance Data (HsBracket GhcPs)
deriving instance Data (HsBracket GhcRn)
deriving instance Data (HsBracket GhcTc)

deriving instance Data (ArithSeqInfo GhcPs)
deriving instance Data (ArithSeqInfo GhcRn)
deriving instance Data (ArithSeqInfo GhcTc)

deriving instance                   Data RecordConTc
deriving instance                   Data CmdTopTc
deriving instance                   Data PendingRnSplice
deriving instance                   Data PendingTcSplice

deriving instance Data (HsMatchContext RdrName)
deriving instance Data (HsMatchContext Name)
deriving instance Data (HsMatchContext Id)

deriving instance Data (HsStmtContext RdrName)
deriving instance Data (HsStmtContext Name)
deriving instance Data (HsStmtContext Id)

-- Data derivations from HsLit ----------------------------------------

deriving instance Data (HsLit GhcPs)
deriving instance Data (HsLit GhcRn)
deriving instance Data (HsLit GhcTc)

deriving instance Data (HsOverLit GhcPs)
deriving instance Data (HsOverLit GhcRn)
deriving instance Data (HsOverLit GhcTc)

-- Data derivations from HsTypes ---------------------------------------

deriving instance Data (LHsQTyVars GhcPs)
deriving instance Data (LHsQTyVars GhcRn)
deriving instance Data (LHsQTyVars GhcTc)

deriving instance (Data thing) => Data (HsImplicitBndrs GhcPs thing)
deriving instance (Data thing) => Data (HsImplicitBndrs GhcRn thing)
deriving instance (Data thing) => Data (HsImplicitBndrs GhcTc thing)

deriving instance (Data thing) => Data (HsWildCardBndrs GhcPs thing)
deriving instance (Data thing) => Data (HsWildCardBndrs GhcRn thing)
deriving instance (Data thing) => Data (HsWildCardBndrs GhcTc thing)

deriving instance Data (HsTyVarBndr GhcPs)
deriving instance Data (HsTyVarBndr GhcRn)
deriving instance Data (HsTyVarBndr GhcTc)

deriving instance Data (HsType GhcPs)
deriving instance Data (HsType GhcRn)
deriving instance Data (HsType GhcTc)

deriving instance Data (HsWildCardInfo GhcPs)
deriving instance Data (HsWildCardInfo GhcRn)
deriving instance Data (HsWildCardInfo GhcTc)

deriving instance Data (HsAppType GhcPs)
deriving instance Data (HsAppType GhcRn)
deriving instance Data (HsAppType GhcTc)

deriving instance Data (ConDeclField GhcPs)
deriving instance Data (ConDeclField GhcRn)
deriving instance Data (ConDeclField GhcTc)

deriving instance Data (FieldOcc GhcPs)
deriving instance Data (FieldOcc GhcRn)
deriving instance Data (FieldOcc GhcTc)

deriving instance Data (AmbiguousFieldOcc GhcPs)
deriving instance Data (AmbiguousFieldOcc GhcRn)
deriving instance Data (AmbiguousFieldOcc GhcTc)

-- Data derivations from HsPat -----------------------------------------

deriving instance Data (Pat GhcPs)
deriving instance Data (Pat GhcRn)
deriving instance Data (Pat GhcTc)

deriving instance (Data body) => Data (HsRecFields GhcPs body)
deriving instance (Data body) => Data (HsRecFields GhcRn body)
deriving instance (Data body) => Data (HsRecFields GhcTc body)

-- Data derivations from HsImpExp --------------------------------------

deriving instance Data (ImportDecl GhcPs)
deriving instance Data (ImportDecl GhcRn)
deriving instance Data (ImportDecl GhcTc)

deriving instance Data (IE GhcPs)
deriving instance Data (IE GhcRn)
deriving instance Data (IE GhcTc)

-- ---------------------------------------------------------------------

