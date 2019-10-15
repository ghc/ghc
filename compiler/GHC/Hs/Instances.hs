{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GHC.Hs.Instances where

-- This module defines the Data instances for the hsSyn AST.

-- It happens here to avoid massive constraint types on the AST with concomitant
-- slow GHC bootstrap times.

-- UndecidableInstances ?

import Data.Data hiding ( Fixity )

import GhcPrelude
import GHC.Hs.Extension
import GHC.Hs.Binds
import GHC.Hs.Decls
import GHC.Hs.Expr
import GHC.Hs.Lit
import GHC.Hs.Types
import GHC.Hs.Pat
import GHC.Hs.ImpExp

-- ---------------------------------------------------------------------
-- Data for abstract families. See Note [Abstract families] in GHC.Hs.Extension.
type DataX = (Data XHsWrapper, Data XTcEvBinds)

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs-----------------------------------------

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Binds ----------------------------------

-- deriving instance (DataIdLR pL pR) => Data (HsLocalBindsLR pL pR)
deriving instance DataX => Data (HsLocalBindsLR GhcPs GhcPs)
deriving instance DataX => Data (HsLocalBindsLR GhcPs GhcRn)
deriving instance DataX => Data (HsLocalBindsLR GhcRn GhcRn)
deriving instance DataX => Data (HsLocalBindsLR GhcTc GhcTc)

-- deriving instance (DataIdLR pL pR) => Data (HsValBindsLR pL pR)
deriving instance DataX => Data (HsValBindsLR GhcPs GhcPs)
deriving instance DataX => Data (HsValBindsLR GhcPs GhcRn)
deriving instance DataX => Data (HsValBindsLR GhcRn GhcRn)
deriving instance DataX => Data (HsValBindsLR GhcTc GhcTc)

-- deriving instance (DataIdLR pL pL) => Data (NHsValBindsLR pL)
deriving instance DataX => Data (NHsValBindsLR GhcPs)
deriving instance DataX => Data (NHsValBindsLR GhcRn)
deriving instance DataX => Data (NHsValBindsLR GhcTc)

-- deriving instance (DataIdLR pL pR) => Data (HsBindLR pL pR)
deriving instance DataX => Data (HsBindLR GhcPs GhcPs)
deriving instance DataX => Data (HsBindLR GhcPs GhcRn)
deriving instance DataX => Data (HsBindLR GhcRn GhcRn)
deriving instance DataX => Data (HsBindLR GhcTc GhcTc)

-- deriving instance (DataId p)       => Data (ABExport p)
deriving instance DataX => Data (ABExport GhcPs)
deriving instance DataX => Data (ABExport GhcRn)
deriving instance DataX => Data (ABExport GhcTc)

-- deriving instance (DataIdLR pL pR) => Data (PatSynBind pL pR)
deriving instance DataX => Data (PatSynBind GhcPs GhcPs)
deriving instance DataX => Data (PatSynBind GhcPs GhcRn)
deriving instance DataX => Data (PatSynBind GhcRn GhcRn)
deriving instance DataX => Data (PatSynBind GhcTc GhcTc)

-- deriving instance (DataIdLR p p)   => Data (HsIPBinds p)
deriving instance DataX => Data (HsIPBinds GhcPs)
deriving instance DataX => Data (HsIPBinds GhcRn)
deriving instance DataX => Data (HsIPBinds GhcTc)

-- deriving instance (DataIdLR p p)   => Data (IPBind p)
deriving instance DataX => Data (IPBind GhcPs)
deriving instance DataX => Data (IPBind GhcRn)
deriving instance DataX => Data (IPBind GhcTc)

-- deriving instance (DataIdLR p p)   => Data (Sig p)
deriving instance DataX => Data (Sig GhcPs)
deriving instance DataX => Data (Sig GhcRn)
deriving instance DataX => Data (Sig GhcTc)

-- deriving instance (DataId p)       => Data (FixitySig p)
deriving instance DataX => Data (FixitySig GhcPs)
deriving instance DataX => Data (FixitySig GhcRn)
deriving instance DataX => Data (FixitySig GhcTc)

-- deriving instance (DataId p)       => Data (StandaloneKindSig p)
deriving instance DataX => Data (StandaloneKindSig GhcPs)
deriving instance DataX => Data (StandaloneKindSig GhcRn)
deriving instance DataX => Data (StandaloneKindSig GhcTc)

-- deriving instance (DataIdLR p p)   => Data (HsPatSynDir p)
deriving instance DataX => Data (HsPatSynDir GhcPs)
deriving instance DataX => Data (HsPatSynDir GhcRn)
deriving instance DataX => Data (HsPatSynDir GhcTc)

deriving instance DataX => Data TcSpecPrag
deriving instance DataX => Data TcSpecPrags

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Decls ----------------------------------

-- deriving instance (DataIdLR p p) => Data (HsDecl p)
deriving instance DataX => Data (HsDecl GhcPs)
deriving instance DataX => Data (HsDecl GhcRn)
deriving instance DataX => Data (HsDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsGroup p)
deriving instance DataX => Data (HsGroup GhcPs)
deriving instance DataX => Data (HsGroup GhcRn)
deriving instance DataX => Data (HsGroup GhcTc)

-- deriving instance (DataIdLR p p) => Data (SpliceDecl p)
deriving instance DataX => Data (SpliceDecl GhcPs)
deriving instance DataX => Data (SpliceDecl GhcRn)
deriving instance DataX => Data (SpliceDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (TyClDecl p)
deriving instance DataX => Data (TyClDecl GhcPs)
deriving instance DataX => Data (TyClDecl GhcRn)
deriving instance DataX => Data (TyClDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (TyClGroup p)
deriving instance DataX => Data (TyClGroup GhcPs)
deriving instance DataX => Data (TyClGroup GhcRn)
deriving instance DataX => Data (TyClGroup GhcTc)

-- deriving instance (DataIdLR p p) => Data (FamilyResultSig p)
deriving instance DataX => Data (FamilyResultSig GhcPs)
deriving instance DataX => Data (FamilyResultSig GhcRn)
deriving instance DataX => Data (FamilyResultSig GhcTc)

-- deriving instance (DataIdLR p p) => Data (FamilyDecl p)
deriving instance DataX => Data (FamilyDecl GhcPs)
deriving instance DataX => Data (FamilyDecl GhcRn)
deriving instance DataX => Data (FamilyDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (InjectivityAnn p)
deriving instance DataX => Data (InjectivityAnn GhcPs)
deriving instance DataX => Data (InjectivityAnn GhcRn)
deriving instance DataX => Data (InjectivityAnn GhcTc)

-- deriving instance (DataIdLR p p) => Data (FamilyInfo p)
deriving instance DataX => Data (FamilyInfo GhcPs)
deriving instance DataX => Data (FamilyInfo GhcRn)
deriving instance DataX => Data (FamilyInfo GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsDataDefn p)
deriving instance DataX => Data (HsDataDefn GhcPs)
deriving instance DataX => Data (HsDataDefn GhcRn)
deriving instance DataX => Data (HsDataDefn GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsDerivingClause p)
deriving instance DataX => Data (HsDerivingClause GhcPs)
deriving instance DataX => Data (HsDerivingClause GhcRn)
deriving instance DataX => Data (HsDerivingClause GhcTc)

-- deriving instance (DataIdLR p p) => Data (ConDecl p)
deriving instance DataX => Data (ConDecl GhcPs)
deriving instance DataX => Data (ConDecl GhcRn)
deriving instance DataX => Data (ConDecl GhcTc)

-- deriving instance DataIdLR p p   => Data (TyFamInstDecl p)
deriving instance DataX => Data (TyFamInstDecl GhcPs)
deriving instance DataX => Data (TyFamInstDecl GhcRn)
deriving instance DataX => Data (TyFamInstDecl GhcTc)

-- deriving instance DataIdLR p p   => Data (DataFamInstDecl p)
deriving instance DataX => Data (DataFamInstDecl GhcPs)
deriving instance DataX => Data (DataFamInstDecl GhcRn)
deriving instance DataX => Data (DataFamInstDecl GhcTc)

-- deriving instance (DataIdLR p p,Data rhs)=>Data (FamEqn p rhs)
deriving instance DataX => Data rhs => Data (FamEqn GhcPs rhs)
deriving instance DataX => Data rhs => Data (FamEqn GhcRn rhs)
deriving instance DataX => Data rhs => Data (FamEqn GhcTc rhs)

-- deriving instance (DataIdLR p p) => Data (ClsInstDecl p)
deriving instance DataX => Data (ClsInstDecl GhcPs)
deriving instance DataX => Data (ClsInstDecl GhcRn)
deriving instance DataX => Data (ClsInstDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (InstDecl p)
deriving instance DataX => Data (InstDecl GhcPs)
deriving instance DataX => Data (InstDecl GhcRn)
deriving instance DataX => Data (InstDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (DerivDecl p)
deriving instance DataX => Data (DerivDecl GhcPs)
deriving instance DataX => Data (DerivDecl GhcRn)
deriving instance DataX => Data (DerivDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (DerivStrategy p)
deriving instance DataX => Data (DerivStrategy GhcPs)
deriving instance DataX => Data (DerivStrategy GhcRn)
deriving instance DataX => Data (DerivStrategy GhcTc)

-- deriving instance (DataIdLR p p) => Data (DefaultDecl p)
deriving instance DataX => Data (DefaultDecl GhcPs)
deriving instance DataX => Data (DefaultDecl GhcRn)
deriving instance DataX => Data (DefaultDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (ForeignDecl p)
deriving instance DataX => Data (ForeignDecl GhcPs)
deriving instance DataX => Data (ForeignDecl GhcRn)
deriving instance DataX => Data (ForeignDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (RuleDecls p)
deriving instance DataX => Data (RuleDecls GhcPs)
deriving instance DataX => Data (RuleDecls GhcRn)
deriving instance DataX => Data (RuleDecls GhcTc)

-- deriving instance (DataIdLR p p) => Data (RuleDecl p)
deriving instance DataX => Data (RuleDecl GhcPs)
deriving instance DataX => Data (RuleDecl GhcRn)
deriving instance DataX => Data (RuleDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (RuleBndr p)
deriving instance DataX => Data (RuleBndr GhcPs)
deriving instance DataX => Data (RuleBndr GhcRn)
deriving instance DataX => Data (RuleBndr GhcTc)

-- deriving instance (DataId p)     => Data (WarnDecls p)
deriving instance DataX => Data (WarnDecls GhcPs)
deriving instance DataX => Data (WarnDecls GhcRn)
deriving instance DataX => Data (WarnDecls GhcTc)

-- deriving instance (DataId p)     => Data (WarnDecl p)
deriving instance DataX => Data (WarnDecl GhcPs)
deriving instance DataX => Data (WarnDecl GhcRn)
deriving instance DataX => Data (WarnDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (AnnDecl p)
deriving instance DataX => Data (AnnDecl GhcPs)
deriving instance DataX => Data (AnnDecl GhcRn)
deriving instance DataX => Data (AnnDecl GhcTc)

-- deriving instance (DataId p)     => Data (RoleAnnotDecl p)
deriving instance DataX => Data (RoleAnnotDecl GhcPs)
deriving instance DataX => Data (RoleAnnotDecl GhcRn)
deriving instance DataX => Data (RoleAnnotDecl GhcTc)

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Expr -----------------------------------

-- deriving instance (DataIdLR p p) => Data (SyntaxExpr p)
deriving instance DataX => Data SyntaxExprTc

-- deriving instance (DataIdLR p p) => Data (HsExpr p)
deriving instance DataX => Data (HsExpr GhcPs)
deriving instance DataX => Data (HsExpr GhcRn)
deriving instance DataX => Data (HsExpr GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsTupArg p)
deriving instance DataX => Data (HsTupArg GhcPs)
deriving instance DataX => Data (HsTupArg GhcRn)
deriving instance DataX => Data (HsTupArg GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsCmd p)
deriving instance DataX => Data (HsCmd GhcPs)
deriving instance DataX => Data (HsCmd GhcRn)
deriving instance DataX => Data (HsCmd GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsCmdTop p)
deriving instance DataX => Data (HsCmdTop GhcPs)
deriving instance DataX => Data (HsCmdTop GhcRn)
deriving instance DataX => Data (HsCmdTop GhcTc)

-- deriving instance (DataIdLR p p,Data body) => Data (MatchGroup p body)
deriving instance (DataX, Data body) => Data (MatchGroup GhcPs body)
deriving instance (DataX, Data body) => Data (MatchGroup GhcRn body)
deriving instance (DataX, Data body) => Data (MatchGroup GhcTc body)

-- deriving instance (DataIdLR p p,Data body) => Data (Match      p body)
deriving instance (DataX, Data body) => Data (Match      GhcPs body)
deriving instance (DataX, Data body) => Data (Match      GhcRn body)
deriving instance (DataX, Data body) => Data (Match      GhcTc body)

-- deriving instance (DataIdLR p p,Data body) => Data (GRHSs      p body)
deriving instance (DataX, Data body) => Data (GRHSs     GhcPs body)
deriving instance (DataX, Data body) => Data (GRHSs     GhcRn body)
deriving instance (DataX, Data body) => Data (GRHSs     GhcTc body)

-- deriving instance (DataIdLR p p,Data body) => Data (GRHS       p body)
deriving instance (DataX, Data body) => Data (GRHS     GhcPs body)
deriving instance (DataX, Data body) => Data (GRHS     GhcRn body)
deriving instance (DataX, Data body) => Data (GRHS     GhcTc body)

-- deriving instance (DataIdLR p p,Data body) => Data (StmtLR   p p body)
deriving instance (DataX, Data body) => Data (StmtLR   GhcPs GhcPs body)
deriving instance (DataX, Data body) => Data (StmtLR   GhcPs GhcRn body)
deriving instance (DataX, Data body) => Data (StmtLR   GhcRn GhcRn body)
deriving instance (DataX, Data body) => Data (StmtLR   GhcTc GhcTc body)

deriving instance DataX => Data RecStmtTc
deriving instance DataX => Data RecordUpdTc
deriving instance (DataX, Data (body GhcTc), Typeable body) => Data (HsWrap body)

-- deriving instance (DataIdLR p p) => Data (ParStmtBlock p p)
deriving instance DataX => Data (ParStmtBlock GhcPs GhcPs)
deriving instance DataX => Data (ParStmtBlock GhcPs GhcRn)
deriving instance DataX => Data (ParStmtBlock GhcRn GhcRn)
deriving instance DataX => Data (ParStmtBlock GhcTc GhcTc)

-- deriving instance (DataIdLR p p) => Data (ApplicativeArg p)
deriving instance DataX => Data (ApplicativeArg GhcPs)
deriving instance DataX => Data (ApplicativeArg GhcRn)
deriving instance DataX => Data (ApplicativeArg GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsSplice p)
deriving instance DataX => Data (HsSplice GhcPs)
deriving instance DataX => Data (HsSplice GhcRn)
deriving instance DataX => Data (HsSplice GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsSplicedThing p)
deriving instance DataX => Data (HsSplicedThing GhcPs)
deriving instance DataX => Data (HsSplicedThing GhcRn)
deriving instance DataX => Data (HsSplicedThing GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsBracket p)
deriving instance DataX => Data (HsBracket GhcPs)
deriving instance DataX => Data (HsBracket GhcRn)
deriving instance DataX => Data (HsBracket GhcTc)

-- deriving instance (DataIdLR p p) => Data (ArithSeqInfo p)
deriving instance DataX => Data (ArithSeqInfo GhcPs)
deriving instance DataX => Data (ArithSeqInfo GhcRn)
deriving instance DataX => Data (ArithSeqInfo GhcTc)

deriving instance DataX =>                   Data RecordConTc
deriving instance DataX =>                   Data CmdTopTc
deriving instance DataX =>                   Data PendingRnSplice
deriving instance DataX =>                   Data PendingTcSplice

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Lit ------------------------------------

-- deriving instance (DataId p) => Data (HsLit p)
deriving instance DataX => Data (HsLit GhcPs)
deriving instance DataX => Data (HsLit GhcRn)
deriving instance DataX => Data (HsLit GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsOverLit p)
deriving instance DataX => Data (HsOverLit GhcPs)
deriving instance DataX => Data (HsOverLit GhcRn)
deriving instance DataX => Data (HsOverLit GhcTc)

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Pat ------------------------------------

-- deriving instance (DataIdLR p p) => Data (Pat p)
deriving instance DataX => Data (Pat GhcPs)
deriving instance DataX => Data (Pat GhcRn)
deriving instance DataX => Data (Pat GhcTc)

deriving instance DataX => Data ListPatTc

-- deriving instance (DataIdLR p p, Data body) => Data (HsRecFields p body)
deriving instance (DataX, Data body) => Data (HsRecFields GhcPs body)
deriving instance (DataX, Data body) => Data (HsRecFields GhcRn body)
deriving instance (DataX, Data body) => Data (HsRecFields GhcTc body)

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Types ----------------------------------

-- deriving instance (DataIdLR p p) => Data (LHsQTyVars p)
deriving instance DataX => Data (LHsQTyVars GhcPs)
deriving instance DataX => Data (LHsQTyVars GhcRn)
deriving instance DataX => Data (LHsQTyVars GhcTc)

-- deriving instance (DataIdLR p p, Data thing) =>Data (HsImplicitBndrs p thing)
deriving instance (DataX, Data thing) => Data (HsImplicitBndrs GhcPs thing)
deriving instance (DataX, Data thing) => Data (HsImplicitBndrs GhcRn thing)
deriving instance (DataX, Data thing) => Data (HsImplicitBndrs GhcTc thing)

-- deriving instance (DataIdLR p p, Data thing) =>Data (HsWildCardBndrs p thing)
deriving instance (DataX, Data thing) => Data (HsWildCardBndrs GhcPs thing)
deriving instance (DataX, Data thing) => Data (HsWildCardBndrs GhcRn thing)
deriving instance (DataX, Data thing) => Data (HsWildCardBndrs GhcTc thing)

-- deriving instance (DataIdLR p p) => Data (HsTyVarBndr p)
deriving instance DataX => Data (HsTyVarBndr GhcPs)
deriving instance DataX => Data (HsTyVarBndr GhcRn)
deriving instance DataX => Data (HsTyVarBndr GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsType p)
deriving instance DataX => Data (HsType GhcPs)
deriving instance DataX => Data (HsType GhcRn)
deriving instance DataX => Data (HsType GhcTc)

deriving instance DataX => Data (LHsTypeArg GhcPs)
deriving instance DataX => Data (LHsTypeArg GhcRn)
deriving instance DataX => Data (LHsTypeArg GhcTc)

-- deriving instance (DataIdLR p p) => Data (ConDeclField p)
deriving instance DataX => Data (ConDeclField GhcPs)
deriving instance DataX => Data (ConDeclField GhcRn)
deriving instance DataX => Data (ConDeclField GhcTc)

-- deriving instance (DataId p)     => Data (FieldOcc p)
deriving instance DataX => Data (FieldOcc GhcPs)
deriving instance DataX => Data (FieldOcc GhcRn)
deriving instance DataX => Data (FieldOcc GhcTc)

-- deriving instance DataId p       => Data (AmbiguousFieldOcc p)
deriving instance DataX => Data (AmbiguousFieldOcc GhcPs)
deriving instance DataX => Data (AmbiguousFieldOcc GhcRn)
deriving instance DataX => Data (AmbiguousFieldOcc GhcTc)


-- deriving instance (DataId name) => Data (ImportDecl name)
deriving instance DataX => Data (ImportDecl GhcPs)
deriving instance DataX => Data (ImportDecl GhcRn)
deriving instance DataX => Data (ImportDecl GhcTc)

-- deriving instance (DataId name)             => Data (IE name)
deriving instance DataX => Data (IE GhcPs)
deriving instance DataX => Data (IE GhcRn)
deriving instance DataX => Data (IE GhcTc)

-- deriving instance (Eq name, Eq (IdP name)) => Eq (IE name)
deriving instance DataX => Eq (IE GhcPs)
deriving instance DataX => Eq (IE GhcRn)
deriving instance DataX => Eq (IE GhcTc)

-- ---------------------------------------------------------------------
