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

-- Data derivations from HsBinds ---------------------------------------

deriving instance (DataIdLR pL pR) => Data (HsLocalBindsLR pL pR)
deriving instance (DataIdLR pL pR) => Data (HsValBindsLR pL pR)
deriving instance (DataIdLR pL pL) => Data (NHsValBindsLR pL)
deriving instance (DataIdLR pL pR) => Data (HsBindLR pL pR)
deriving instance (DataId p)       => Data (ABExport p)
deriving instance (DataIdLR pL pR) => Data (PatSynBind pL pR)
deriving instance (DataIdLR p p)   => Data (HsIPBinds p)
deriving instance (DataIdLR p p)   => Data (IPBind p)
deriving instance (DataIdLR p p)   => Data (Sig p)
deriving instance (DataId p)       => Data (FixitySig p)
deriving instance (DataIdLR p p)   => Data (HsPatSynDir p)

-- Data derivations from HsDecls ---------------------------------------

deriving instance (DataIdLR p p) => Data (HsDecl p)
deriving instance (DataIdLR p p) => Data (HsGroup p)
deriving instance (DataIdLR p p) => Data (SpliceDecl p)
deriving instance (DataIdLR p p) => Data (TyClDecl p)
deriving instance (DataIdLR p p) => Data (TyClGroup p)
deriving instance (DataIdLR p p) => Data (FamilyResultSig p)
deriving instance (DataIdLR p p) => Data (FamilyDecl p)
deriving instance (DataIdLR p p) => Data (InjectivityAnn p)
deriving instance (DataIdLR p p) => Data (FamilyInfo p)
deriving instance (DataIdLR p p) => Data (HsDataDefn p)
deriving instance (DataIdLR p p) => Data (HsDerivingClause p)
deriving instance (DataIdLR p p) => Data (ConDecl p)
deriving instance DataIdLR p p   => Data (TyFamInstDecl p)
deriving instance DataIdLR p p   => Data (DataFamInstDecl p)
deriving instance (DataIdLR p p,Data pats,Data rhs) => Data (FamEqn p pats rhs)
deriving instance (DataIdLR p p) => Data (ClsInstDecl p)
deriving instance (DataIdLR p p) => Data (InstDecl p)
deriving instance (DataIdLR p p) => Data (DerivDecl p)
deriving instance (DataIdLR p p) => Data (DefaultDecl p)
deriving instance (DataIdLR p p) => Data (ForeignDecl p)
deriving instance (DataIdLR p p) => Data (RuleDecls p)
deriving instance (DataIdLR p p) => Data (RuleDecl p)
deriving instance (DataIdLR p p) => Data (RuleBndr p)
deriving instance (DataIdLR p p) => Data (VectDecl p)
deriving instance (DataId p)     => Data (WarnDecls p)
deriving instance (DataId p)     => Data (WarnDecl p)
deriving instance (DataIdLR p p) => Data (AnnDecl p)
deriving instance (DataId p)     => Data (RoleAnnotDecl p)

-- Data derivations from HsExpr ----------------------------------------

deriving instance (DataIdLR p p) => Data (SyntaxExpr p)
deriving instance (DataIdLR p p) => Data (HsExpr p)
deriving instance (DataIdLR p p) => Data (HsTupArg p)
deriving instance (DataIdLR p p) => Data (HsCmd p)
deriving instance (DataIdLR p p) => Data (HsCmdTop p)
deriving instance (DataIdLR p p,Data body) => Data (MatchGroup p body)
deriving instance (DataIdLR p p,Data body) => Data (Match      p body)
deriving instance (DataIdLR p p,Data body) => Data (GRHSs      p body)
deriving instance (DataIdLR p p,Data body) => Data (GRHS       p body)
deriving instance (DataIdLR p p,Data body) => Data (StmtLR   p p body)
deriving instance (DataIdLR p p) => Data (ParStmtBlock p p)
deriving instance (DataIdLR p p) => Data (ApplicativeArg p)
deriving instance (DataIdLR p p) => Data (HsSplice p)
deriving instance (DataIdLR p p) => Data (HsSplicedThing p)
deriving instance (DataIdLR p p) => Data (HsBracket p)
deriving instance (DataIdLR p p) => Data (ArithSeqInfo p)
deriving instance                   Data RecordConTc
deriving instance                   Data CmdTopTc
deriving instance                   Data PendingRnSplice
deriving instance                   Data PendingTcSplice

-- Data derivations from HsLit ----------------------------------------

deriving instance (DataId p) => Data (HsLit p)
deriving instance (DataIdLR p p) => Data (HsOverLit p)

-- Data derivations from HsPat -----------------------------------------

deriving instance (DataIdLR p p) => Data (Pat p)
deriving instance (DataIdLR p p, Data body) => Data (HsRecFields p body)

-- Data derivations from HsTypes ---------------------------------------

deriving instance (DataIdLR p p) => Data (LHsQTyVars p)
deriving instance (DataIdLR p p, Data thing) => Data (HsImplicitBndrs p thing)
deriving instance (DataIdLR p p, Data thing) => Data (HsWildCardBndrs p thing)
deriving instance (DataIdLR p p) => Data (HsTyVarBndr p)
deriving instance (DataIdLR p p) => Data (HsType p)
deriving instance (DataId p)     => Data (HsWildCardInfo p)
deriving instance (DataIdLR p p) => Data (HsAppType p)
deriving instance (DataIdLR p p) => Data (ConDeclField p)
deriving instance (DataId p)     => Data (FieldOcc p)
deriving instance DataId p       => Data (AmbiguousFieldOcc p)

-- ---------------------------------------------------------------------
