{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
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
-- Data derivations from GHC.Hs-----------------------------------------

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Binds ----------------------------------

-- deriving instance (DataIdLR pL pR) => Data (HsLocalBindsLR pL pR)
deriving instance Data (HsLocalBindsLR GhcPs GhcPs)
deriving instance Data (HsLocalBindsLR GhcPs GhcRn)
deriving instance Data (HsLocalBindsLR GhcRn GhcRn)
deriving instance Data (HsLocalBindsLR GhcTc GhcTc)

-- deriving instance (DataIdLR pL pR) => Data (HsValBindsLR pL pR)
deriving instance Data (HsValBindsLR GhcPs GhcPs)
deriving instance Data (HsValBindsLR GhcPs GhcRn)
deriving instance Data (HsValBindsLR GhcRn GhcRn)
deriving instance Data (HsValBindsLR GhcTc GhcTc)

-- deriving instance (DataIdLR pL pL) => Data (NHsValBindsLR pL)
deriving instance Data (NHsValBindsLR GhcPs)
deriving instance Data (NHsValBindsLR GhcRn)
deriving instance Data (NHsValBindsLR GhcTc)

-- deriving instance (DataIdLR pL pR) => Data (HsBindLR pL pR)
deriving instance Data (HsBindLR GhcPs GhcPs)
deriving instance Data (HsBindLR GhcPs GhcRn)
deriving instance Data (HsBindLR GhcRn GhcRn)
deriving instance Data (HsBindLR GhcTc GhcTc)

-- deriving instance (DataId p)       => Data (ABExport p)
deriving instance Data (ABExport GhcPs)
deriving instance Data (ABExport GhcRn)
deriving instance Data (ABExport GhcTc)

-- deriving instance (DataIdLR pL pR) => Data (PatSynBind pL pR)
deriving instance Data (PatSynBind GhcPs GhcPs)
deriving instance Data (PatSynBind GhcPs GhcRn)
deriving instance Data (PatSynBind GhcRn GhcRn)
deriving instance Data (PatSynBind GhcTc GhcTc)

-- deriving instance (DataIdLR p p)   => Data (HsIPBinds p)
deriving instance Data (HsIPBinds GhcPs)
deriving instance Data (HsIPBinds GhcRn)
deriving instance Data (HsIPBinds GhcTc)

-- deriving instance (DataIdLR p p)   => Data (IPBind p)
deriving instance Data (IPBind GhcPs)
deriving instance Data (IPBind GhcRn)
deriving instance Data (IPBind GhcTc)

-- deriving instance (DataIdLR p p)   => Data (Sig p)
deriving instance Data (Sig GhcPs)
deriving instance Data (Sig GhcRn)
deriving instance Data (Sig GhcTc)

-- deriving instance (DataId p)       => Data (FixitySig p)
deriving instance Data (FixitySig GhcPs)
deriving instance Data (FixitySig GhcRn)
deriving instance Data (FixitySig GhcTc)

-- deriving instance (DataId p)       => Data (StandaloneKindSig p)
deriving instance Data (StandaloneKindSig GhcPs)
deriving instance Data (StandaloneKindSig GhcRn)
deriving instance Data (StandaloneKindSig GhcTc)

-- deriving instance (DataIdLR p p)   => Data (HsPatSynDir p)
deriving instance Data (HsPatSynDir GhcPs)
deriving instance Data (HsPatSynDir GhcRn)
deriving instance Data (HsPatSynDir GhcTc)

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Decls ----------------------------------

-- deriving instance (DataIdLR p p) => Data (HsDecl p)
deriving instance Data (HsDecl GhcPs)
deriving instance Data (HsDecl GhcRn)
deriving instance Data (HsDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsGroup p)
deriving instance Data (HsGroup GhcPs)
deriving instance Data (HsGroup GhcRn)
deriving instance Data (HsGroup GhcTc)

-- deriving instance (DataIdLR p p) => Data (SpliceDecl p)
deriving instance Data (SpliceDecl GhcPs)
deriving instance Data (SpliceDecl GhcRn)
deriving instance Data (SpliceDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (TyClDecl p)
deriving instance Data (TyClDecl GhcPs)
deriving instance Data (TyClDecl GhcRn)
deriving instance Data (TyClDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (TyClGroup p)
deriving instance Data (TyClGroup GhcPs)
deriving instance Data (TyClGroup GhcRn)
deriving instance Data (TyClGroup GhcTc)

-- deriving instance (DataIdLR p p) => Data (FamilyResultSig p)
deriving instance Data (FamilyResultSig GhcPs)
deriving instance Data (FamilyResultSig GhcRn)
deriving instance Data (FamilyResultSig GhcTc)

-- deriving instance (DataIdLR p p) => Data (FamilyDecl p)
deriving instance Data (FamilyDecl GhcPs)
deriving instance Data (FamilyDecl GhcRn)
deriving instance Data (FamilyDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (InjectivityAnn p)
deriving instance Data (InjectivityAnn GhcPs)
deriving instance Data (InjectivityAnn GhcRn)
deriving instance Data (InjectivityAnn GhcTc)

-- deriving instance (DataIdLR p p) => Data (FamilyInfo p)
deriving instance Data (FamilyInfo GhcPs)
deriving instance Data (FamilyInfo GhcRn)
deriving instance Data (FamilyInfo GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsDataDefn p)
deriving instance Data (HsDataDefn GhcPs)
deriving instance Data (HsDataDefn GhcRn)
deriving instance Data (HsDataDefn GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsDerivingClause p)
deriving instance Data (HsDerivingClause GhcPs)
deriving instance Data (HsDerivingClause GhcRn)
deriving instance Data (HsDerivingClause GhcTc)

-- deriving instance (DataIdLR p p) => Data (ConDecl p)
deriving instance Data (ConDecl GhcPs)
deriving instance Data (ConDecl GhcRn)
deriving instance Data (ConDecl GhcTc)

-- deriving instance DataIdLR p p   => Data (TyFamInstDecl p)
deriving instance Data (TyFamInstDecl GhcPs)
deriving instance Data (TyFamInstDecl GhcRn)
deriving instance Data (TyFamInstDecl GhcTc)

-- deriving instance DataIdLR p p   => Data (DataFamInstDecl p)
deriving instance Data (DataFamInstDecl GhcPs)
deriving instance Data (DataFamInstDecl GhcRn)
deriving instance Data (DataFamInstDecl GhcTc)

-- deriving instance (DataIdLR p p,Data rhs)=>Data (FamEqn p rhs)
deriving instance Data rhs => Data (FamEqn GhcPs rhs)
deriving instance Data rhs => Data (FamEqn GhcRn rhs)
deriving instance Data rhs => Data (FamEqn GhcTc rhs)

-- deriving instance (DataIdLR p p) => Data (ClsInstDecl p)
deriving instance Data (ClsInstDecl GhcPs)
deriving instance Data (ClsInstDecl GhcRn)
deriving instance Data (ClsInstDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (InstDecl p)
deriving instance Data (InstDecl GhcPs)
deriving instance Data (InstDecl GhcRn)
deriving instance Data (InstDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (DerivDecl p)
deriving instance Data (DerivDecl GhcPs)
deriving instance Data (DerivDecl GhcRn)
deriving instance Data (DerivDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (DerivStrategy p)
deriving instance Data (DerivStrategy GhcPs)
deriving instance Data (DerivStrategy GhcRn)
deriving instance Data (DerivStrategy GhcTc)

-- deriving instance (DataIdLR p p) => Data (DefaultDecl p)
deriving instance Data (DefaultDecl GhcPs)
deriving instance Data (DefaultDecl GhcRn)
deriving instance Data (DefaultDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (ForeignDecl p)
deriving instance Data (ForeignDecl GhcPs)
deriving instance Data (ForeignDecl GhcRn)
deriving instance Data (ForeignDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (RuleDecls p)
deriving instance Data (RuleDecls GhcPs)
deriving instance Data (RuleDecls GhcRn)
deriving instance Data (RuleDecls GhcTc)

-- deriving instance (DataIdLR p p) => Data (RuleDecl p)
deriving instance Data (RuleDecl GhcPs)
deriving instance Data (RuleDecl GhcRn)
deriving instance Data (RuleDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (RuleBndr p)
deriving instance Data (RuleBndr GhcPs)
deriving instance Data (RuleBndr GhcRn)
deriving instance Data (RuleBndr GhcTc)

-- deriving instance (DataId p)     => Data (WarnDecls p)
deriving instance Data (WarnDecls GhcPs)
deriving instance Data (WarnDecls GhcRn)
deriving instance Data (WarnDecls GhcTc)

-- deriving instance (DataId p)     => Data (WarnDecl p)
deriving instance Data (WarnDecl GhcPs)
deriving instance Data (WarnDecl GhcRn)
deriving instance Data (WarnDecl GhcTc)

-- deriving instance (DataIdLR p p) => Data (AnnDecl p)
deriving instance Data (AnnDecl GhcPs)
deriving instance Data (AnnDecl GhcRn)
deriving instance Data (AnnDecl GhcTc)

-- deriving instance (DataId p)     => Data (RoleAnnotDecl p)
deriving instance Data (RoleAnnotDecl GhcPs)
deriving instance Data (RoleAnnotDecl GhcRn)
deriving instance Data (RoleAnnotDecl GhcTc)

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Expr -----------------------------------

-- deriving instance (DataIdLR p p) => Data (HsPragE p)
deriving instance Data (HsPragE GhcPs)
deriving instance Data (HsPragE GhcRn)
deriving instance Data (HsPragE GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsExpr p)
deriving instance Data (HsExpr GhcPs)
deriving instance Data (HsExpr GhcRn)
deriving instance Data (HsExpr GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsTupArg p)
deriving instance Data (HsTupArg GhcPs)
deriving instance Data (HsTupArg GhcRn)
deriving instance Data (HsTupArg GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsCmd p)
deriving instance Data (HsCmd GhcPs)
deriving instance Data (HsCmd GhcRn)
deriving instance Data (HsCmd GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsCmdTop p)
deriving instance Data (HsCmdTop GhcPs)
deriving instance Data (HsCmdTop GhcRn)
deriving instance Data (HsCmdTop GhcTc)

-- deriving instance (DataIdLR p p,Data body) => Data (MatchGroup p body)
deriving instance (Data body) => Data (MatchGroup GhcPs body)
deriving instance (Data body) => Data (MatchGroup GhcRn body)
deriving instance (Data body) => Data (MatchGroup GhcTc body)

-- deriving instance (DataIdLR p p,Data body) => Data (Match      p body)
deriving instance (Data body) => Data (Match      GhcPs body)
deriving instance (Data body) => Data (Match      GhcRn body)
deriving instance (Data body) => Data (Match      GhcTc body)

-- deriving instance (DataIdLR p p,Data body) => Data (GRHSs      p body)
deriving instance (Data body) => Data (GRHSs     GhcPs body)
deriving instance (Data body) => Data (GRHSs     GhcRn body)
deriving instance (Data body) => Data (GRHSs     GhcTc body)

-- deriving instance (DataIdLR p p,Data body) => Data (GRHS       p body)
deriving instance (Data body) => Data (GRHS     GhcPs body)
deriving instance (Data body) => Data (GRHS     GhcRn body)
deriving instance (Data body) => Data (GRHS     GhcTc body)

-- deriving instance (DataIdLR p p,Data body) => Data (StmtLR   p p body)
deriving instance (Data body) => Data (StmtLR   GhcPs GhcPs body)
deriving instance (Data body) => Data (StmtLR   GhcPs GhcRn body)
deriving instance (Data body) => Data (StmtLR   GhcRn GhcRn body)
deriving instance (Data body) => Data (StmtLR   GhcTc GhcTc body)

deriving instance Data RecStmtTc

-- deriving instance (DataIdLR p p) => Data (ParStmtBlock p p)
deriving instance Data (ParStmtBlock GhcPs GhcPs)
deriving instance Data (ParStmtBlock GhcPs GhcRn)
deriving instance Data (ParStmtBlock GhcRn GhcRn)
deriving instance Data (ParStmtBlock GhcTc GhcTc)

-- deriving instance (DataIdLR p p) => Data (ApplicativeArg p)
deriving instance Data (ApplicativeArg GhcPs)
deriving instance Data (ApplicativeArg GhcRn)
deriving instance Data (ApplicativeArg GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsSplice p)
deriving instance Data (HsSplice GhcPs)
deriving instance Data (HsSplice GhcRn)
deriving instance Data (HsSplice GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsSplicedThing p)
deriving instance Data (HsSplicedThing GhcPs)
deriving instance Data (HsSplicedThing GhcRn)
deriving instance Data (HsSplicedThing GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsBracket p)
deriving instance Data (HsBracket GhcPs)
deriving instance Data (HsBracket GhcRn)
deriving instance Data (HsBracket GhcTc)

-- deriving instance (DataIdLR p p) => Data (ArithSeqInfo p)
deriving instance Data (ArithSeqInfo GhcPs)
deriving instance Data (ArithSeqInfo GhcRn)
deriving instance Data (ArithSeqInfo GhcTc)

deriving instance Data RecordConTc
deriving instance Data RecordUpdTc
deriving instance Data CmdTopTc
deriving instance Data PendingRnSplice
deriving instance Data PendingTcSplice
deriving instance Data SyntaxExprRn
deriving instance Data SyntaxExprTc

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Lit ------------------------------------

-- deriving instance (DataId p) => Data (HsLit p)
deriving instance Data (HsLit GhcPs)
deriving instance Data (HsLit GhcRn)
deriving instance Data (HsLit GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsOverLit p)
deriving instance Data (HsOverLit GhcPs)
deriving instance Data (HsOverLit GhcRn)
deriving instance Data (HsOverLit GhcTc)

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Pat ------------------------------------

-- deriving instance (DataIdLR p p) => Data (Pat p)
deriving instance Data (Pat GhcPs)
deriving instance Data (Pat GhcRn)
deriving instance Data (Pat GhcTc)

deriving instance Data CoPat
deriving instance Data ConPatTc

deriving instance Data ListPatTc

-- deriving instance (DataIdLR p p, Data body) => Data (HsRecFields p body)
deriving instance (Data body) => Data (HsRecFields GhcPs body)
deriving instance (Data body) => Data (HsRecFields GhcRn body)
deriving instance (Data body) => Data (HsRecFields GhcTc body)

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Types ----------------------------------

-- deriving instance (DataIdLR p p) => Data (LHsQTyVars p)
deriving instance Data (LHsQTyVars GhcPs)
deriving instance Data (LHsQTyVars GhcRn)
deriving instance Data (LHsQTyVars GhcTc)

-- deriving instance (DataIdLR p p, Data thing) =>Data (HsImplicitBndrs p thing)
deriving instance (Data thing) => Data (HsImplicitBndrs GhcPs thing)
deriving instance (Data thing) => Data (HsImplicitBndrs GhcRn thing)
deriving instance (Data thing) => Data (HsImplicitBndrs GhcTc thing)

-- deriving instance (DataIdLR p p, Data thing) =>Data (HsWildCardBndrs p thing)
deriving instance (Data thing) => Data (HsWildCardBndrs GhcPs thing)
deriving instance (Data thing) => Data (HsWildCardBndrs GhcRn thing)
deriving instance (Data thing) => Data (HsWildCardBndrs GhcTc thing)

-- deriving instance (DataIdLR p p) => Data (HsTyVarBndr p)
deriving instance Data (HsTyVarBndr GhcPs)
deriving instance Data (HsTyVarBndr GhcRn)
deriving instance Data (HsTyVarBndr GhcTc)

-- deriving instance (DataIdLR p p) => Data (HsType p)
deriving instance Data (HsType GhcPs)
deriving instance Data (HsType GhcRn)
deriving instance Data (HsType GhcTc)

deriving instance Data (LHsTypeArg GhcPs)
deriving instance Data (LHsTypeArg GhcRn)
deriving instance Data (LHsTypeArg GhcTc)

-- deriving instance (DataIdLR p p) => Data (ConDeclField p)
deriving instance Data (ConDeclField GhcPs)
deriving instance Data (ConDeclField GhcRn)
deriving instance Data (ConDeclField GhcTc)

-- deriving instance (DataId p)     => Data (FieldOcc p)
deriving instance Data (FieldOcc GhcPs)
deriving instance Data (FieldOcc GhcRn)
deriving instance Data (FieldOcc GhcTc)

-- deriving instance DataId p       => Data (AmbiguousFieldOcc p)
deriving instance Data (AmbiguousFieldOcc GhcPs)
deriving instance Data (AmbiguousFieldOcc GhcRn)
deriving instance Data (AmbiguousFieldOcc GhcTc)


-- deriving instance (DataId name) => Data (ImportDecl name)
deriving instance Data (ImportDecl GhcPs)
deriving instance Data (ImportDecl GhcRn)
deriving instance Data (ImportDecl GhcTc)

-- deriving instance (DataId name)             => Data (IE name)
deriving instance Data (IE GhcPs)
deriving instance Data (IE GhcRn)
deriving instance Data (IE GhcTc)

-- deriving instance (Eq name, Eq (IdP name)) => Eq (IE name)
deriving instance Eq (IE GhcPs)
deriving instance Eq (IE GhcRn)
deriving instance Eq (IE GhcTc)

-- ---------------------------------------------------------------------
