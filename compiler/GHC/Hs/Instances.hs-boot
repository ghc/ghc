{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This module contains exclusively Data instances, which are going to be slow
-- no matter what we do. Furthermore, they are incredibly slow to compile with
-- optimisation (see #9557). Consequently we compile this with -O0.
-- See #18254.
{-# OPTIONS_GHC -O0 #-}

module GHC.Hs.Instances where

-- This module defines the Data instances for the hsSyn AST.

-- It happens here to avoid massive constraint types on the AST with concomitant
-- slow GHC bootstrap times.

-- UndecidableInstances ?

{- Note [Data.Data instances for GHC AST Types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We give all of the frontend types and their instantiations (HsSyn) and
some other types Data.Data instances. There are two main motivations to
do so:

* For users of the GHC API it allows to write Generic code over the GHC AST.
* GHC itself has a few uses of these as well:
    * In the showAstData, showAstDataFull helpers to print a representation of
      the actual AST using it's constructors rather than just user facing pretty printing.
    * It's used to some degree for HIE file generation in the ToHIE instances.
    * TH serialization uses it for serialization of Annotations (GHC.Serialized)
    * Some of the dump flags use showAstData to produce the actual dump output.
-}

import Data.Data hiding ( Fixity )

import GHC.Prelude
import GHC.Hs.Extension
import GHC.Hs.Binds
import GHC.Hs.Decls
import GHC.Hs.Expr
import GHC.Hs.Lit
import GHC.Hs.Type
import GHC.Hs.Pat
import GHC.Hs.ImpExp
import GHC.Parser.Annotation
import GHC.Types.Name.Reader (WithUserRdr(..))
import GHC.Types.InlinePragma (ActivationGhc)
import GHC.Data.BooleanFormula (BooleanFormula(..))
import Language.Haskell.Syntax.Decls
import Language.Haskell.Syntax.Decls.Foreign (CType(..), Header(..))
import Language.Haskell.Syntax.Decls.Overlap (OverlapMode(..))
import Language.Haskell.Syntax.Extension (Anno)
import Language.Haskell.Syntax.Binds.InlinePragma (ActivationX(..), InlinePragma(..))
import GHC.Tc.Types.ErrCtxt

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs-----------------------------------------

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Binds ----------------------------------

-- instance (DataIdLR pL pR) => Data (HsLocalBindsLR pL pR)
instance Data (HsLocalBindsLR GhcPs GhcPs)
instance Data (HsLocalBindsLR GhcPs GhcRn)
instance Data (HsLocalBindsLR GhcRn GhcRn)
instance Data (HsLocalBindsLR GhcTc GhcTc)

-- instance (DataIdLR pL pR) => Data (HsValBindsLR pL pR)
instance Data (HsValBindsLR GhcPs GhcPs)
instance Data (HsValBindsLR GhcPs GhcRn)
instance Data (HsValBindsLR GhcRn GhcRn)
instance Data (HsValBindsLR GhcTc GhcTc)

-- instance (DataIdLR pL pL) => Data (NHsValBindsLR pL)
instance Data (HsValBindGroups 'Parsed)
instance Data (HsValBindGroups 'Renamed)
instance Data (HsValBindGroups 'Typechecked)

-- instance (DataIdLR pL pR) => Data (HsBindLR pL pR)
instance Data (HsBindLR GhcPs GhcPs)
instance Data (HsBindLR GhcPs GhcRn)
instance Data (HsBindLR GhcRn GhcRn)
instance Data (HsBindLR GhcTc GhcTc)

instance Data XPatBindTc

instance Data AbsBinds

instance Data ABExport

-- instance DataId p => Data (RecordPatSynField p)
instance Data (RecordPatSynField GhcPs)
instance Data (RecordPatSynField GhcRn)
instance Data (RecordPatSynField GhcTc)

-- instance (DataIdLR pL pR) => Data (PatSynBind pL pR)
instance Data (PatSynBind GhcPs GhcPs)
instance Data (PatSynBind GhcPs GhcRn)
instance Data (PatSynBind GhcRn GhcRn)
instance Data (PatSynBind GhcTc GhcTc)

-- instance (DataIdLR p p)   => Data (HsIPBinds p)
instance Data (HsIPBinds GhcPs)
instance Data (HsIPBinds GhcRn)
instance Data (HsIPBinds GhcTc)

-- instance (DataIdLR p p)   => Data (IPBind p)
instance Data (IPBind GhcPs)
instance Data (IPBind GhcRn)
instance Data (IPBind GhcTc)

-- instance (DataIdLR p p)   => Data (Sig p)
instance Data (Sig GhcPs)
instance Data (Sig GhcRn)
instance Data (Sig GhcTc)

-- instance (DataId p)       => Data (FixitySig p)
instance Data (FixitySig GhcPs)
instance Data (FixitySig GhcRn)
instance Data (FixitySig GhcTc)

-- instance (DataId p)       => Data (StandaloneKindSig p)
instance Data (StandaloneKindSig GhcPs)
instance Data (StandaloneKindSig GhcRn)
instance Data (StandaloneKindSig GhcTc)

-- instance (DataIdLR p p)   => Data (HsPatSynDir p)
instance Data (HsPatSynDir GhcPs)
instance Data (HsPatSynDir GhcRn)
instance Data (HsPatSynDir GhcTc)

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Decls ----------------------------------

-- instance (DataIdLR p p) => Data (HsDecl p)
instance Data (HsDecl GhcPs)
instance Data (HsDecl GhcRn)
instance Data (HsDecl GhcTc)

-- instance (DataIdLR p p) => Data (HsGroup p)
instance Data (HsGroup GhcPs)
instance Data (HsGroup GhcRn)
instance Data (HsGroup GhcTc)

-- instance (DataIdLR p p) => Data (SpliceDecl p)
instance Data (SpliceDecl GhcPs)
instance Data (SpliceDecl GhcRn)
instance Data (SpliceDecl GhcTc)

-- instance (DataIdLR p p) => Data (TyClDecl p)
instance Data (TyClDecl GhcPs)
instance Data (TyClDecl GhcRn)
instance Data (TyClDecl GhcTc)

-- instance (DataIdLR p p) => Data (FunDep p)
instance Data (FunDep GhcPs)
instance Data (FunDep GhcRn)
instance Data (FunDep GhcTc)

-- instance (DataIdLR p p) => Data (TyClGroup p)
instance Data (TyClGroup GhcPs)
instance Data (TyClGroup GhcRn)
instance Data (TyClGroup GhcTc)

-- instance (DataIdLR p p) => Data (FamilyResultSig p)
instance Data (FamilyResultSig GhcPs)
instance Data (FamilyResultSig GhcRn)
instance Data (FamilyResultSig GhcTc)

-- instance (DataIdLR p p) => Data (FamilyDecl p)
instance Data (FamilyDecl GhcPs)
instance Data (FamilyDecl GhcRn)
instance Data (FamilyDecl GhcTc)

-- instance (DataIdLR p p) => Data (InjectivityAnn p)
instance Data (InjectivityAnn GhcPs)
instance Data (InjectivityAnn GhcRn)
instance Data (InjectivityAnn GhcTc)

-- instance (DataIdLR p p) => Data (FamilyInfo p)
instance Data (FamilyInfo GhcPs)
instance Data (FamilyInfo GhcRn)
instance Data (FamilyInfo GhcTc)

-- instance (DataIdLR p p) => Data (HsDataDefn p)
instance Data (HsDataDefn GhcPs)
instance Data (HsDataDefn GhcRn)
instance Data (HsDataDefn GhcTc)

-- instance (DataIdLR p p) => Data (HsDerivingClause p)
instance Data (HsDerivingClause GhcPs)
instance Data (HsDerivingClause GhcRn)
instance Data (HsDerivingClause GhcTc)

-- instance DataIdLR p p => Data (DerivClauseTys p)
instance Data (DerivClauseTys GhcPs)
instance Data (DerivClauseTys GhcRn)
instance Data (DerivClauseTys GhcTc)

-- instance (DataIdLR p p) => Data (ConDecl p)
instance Data (ConDecl GhcPs)
instance Data (ConDecl GhcRn)
instance Data (ConDecl GhcTc)

-- instance DataIdLR p p => Data (HsConDeclGADTDetails p)
instance Data (HsConDeclGADTDetails GhcPs)
instance Data (HsConDeclGADTDetails GhcRn)
instance Data (HsConDeclGADTDetails GhcTc)

-- instance DataIdLR p p   => Data (TyFamInstDecl p)
instance Data (TyFamInstDecl GhcPs)
instance Data (TyFamInstDecl GhcRn)
instance Data (TyFamInstDecl GhcTc)

-- instance DataIdLR p p   => Data (DataFamInstDecl p)
instance Data (DataFamInstDecl GhcPs)
instance Data (DataFamInstDecl GhcRn)
instance Data (DataFamInstDecl GhcTc)

-- instance (DataIdLR p p,Data rhs)=>Data (FamEqn p rhs)
instance Data rhs => Data (FamEqn GhcPs rhs)
instance Data rhs => Data (FamEqn GhcRn rhs)
instance Data rhs => Data (FamEqn GhcTc rhs)

-- instance (DataIdLR p p) => Data (ClsInstDecl p)
instance Data (ClsInstDecl GhcPs)
instance Data (ClsInstDecl GhcRn)
instance Data (ClsInstDecl GhcTc)

-- instance (DataIdLR p p) => Data (InstDecl p)
instance Data (InstDecl GhcPs)
instance Data (InstDecl GhcRn)
instance Data (InstDecl GhcTc)

-- instance (DataIdLR p p) => Data (DerivDecl p)
instance Data (DerivDecl GhcPs)
instance Data (DerivDecl GhcRn)
instance Data (DerivDecl GhcTc)

-- instance (DataIdLR p p) => Data (DerivStrategy p)
instance Data (DerivStrategy GhcPs)
instance Data (DerivStrategy GhcRn)
instance Data (DerivStrategy GhcTc)

-- instance (DataIdLR p p) => Data (DefaultDecl p)
instance Data (DefaultDecl GhcPs)
instance Data (DefaultDecl GhcRn)
instance Data (DefaultDecl GhcTc)

-- instance (DataIdLR p p) => Data (ForeignDecl p)
instance Data (ForeignDecl GhcPs)
instance Data (ForeignDecl GhcRn)
instance Data (ForeignDecl GhcTc)

-- instance (DataIdLR p p) => Data (ForeignImport p)
instance Data (ForeignImport GhcPs)
instance Data (ForeignImport GhcRn)
instance Data (ForeignImport GhcTc)

-- instance (DataIdLR p p) => Data (ForeignExport p)
instance Data (ForeignExport GhcPs)
instance Data (ForeignExport GhcRn)
instance Data (ForeignExport GhcTc)

-- instance (DataIdLR p p) => Data (CImportSpec p)
instance Data (CImportSpec GhcPs)
instance Data (CImportSpec GhcRn)
instance Data (CImportSpec GhcTc)

-- instance (DataIdLR p p) => Data (CCallTarget p)
instance Data (CCallTarget GhcPs)
instance Data (CCallTarget GhcRn)
instance Data (CCallTarget GhcTc)

-- instance (DataIdLR p p) => Data (CType p)
instance Data (CType GhcPs)
instance Data (CType GhcRn)
instance Data (CType GhcTc)

-- instance (DataIdLR p p) => Data (Header p)
instance Data (Header GhcPs)
instance Data (Header GhcRn)
instance Data (Header GhcTc)

-- instance (DataIdLR p p) => Data (RuleDecls p)
instance Data (RuleDecls GhcPs)
instance Data (RuleDecls GhcRn)
instance Data (RuleDecls GhcTc)

-- instance (DataIdLR p p) => Data (RuleDecl p)
instance Data (RuleDecl GhcPs)
instance Data (RuleDecl GhcRn)
instance Data (RuleDecl GhcTc)

-- instance (DataIdLR p p) => Data (RuleBndr p)
instance Data (RuleBndr GhcPs)
instance Data (RuleBndr GhcRn)
instance Data (RuleBndr GhcTc)

instance Data (RuleBndrs GhcPs)
instance Data (RuleBndrs GhcRn)
instance Data (RuleBndrs GhcTc)

instance Data TcSpecPrags
instance Data TcSpecPrag

-- instance (DataId p)     => Data (WarnDecls p)
instance Data (WarnDecls GhcPs)
instance Data (WarnDecls GhcRn)
instance Data (WarnDecls GhcTc)

-- instance (DataId p)     => Data (WarnDecl p)
instance Data (WarnDecl GhcPs)
instance Data (WarnDecl GhcRn)
instance Data (WarnDecl GhcTc)

instance Data (WarningTxt GhcPs)
instance Data (WarningTxt GhcRn)
instance Data (WarningTxt GhcTc)

instance Data (InWarningCategory GhcPs)
instance Data (InWarningCategory GhcRn)
instance Data (InWarningCategory GhcTc)

-- instance (DataIdLR p p) => Data (AnnDecl p)
instance Data (AnnProvenance GhcPs)
instance Data (AnnProvenance GhcRn)
instance Data (AnnProvenance GhcTc)

instance Data (AnnDecl GhcPs)
instance Data (AnnDecl GhcRn)
instance Data (AnnDecl GhcTc)

-- instance (DataId p)     => Data (RoleAnnotDecl p)
instance Data (RoleAnnotDecl GhcPs)
instance Data (RoleAnnotDecl GhcRn)
instance Data (RoleAnnotDecl GhcTc)

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Expr -----------------------------------

instance Data (FieldLabelStrings GhcPs)
instance Data (FieldLabelStrings GhcRn)
instance Data (FieldLabelStrings GhcTc)

instance Data (HsRecUpdParent GhcPs)
instance Data (HsRecUpdParent GhcRn)
instance Data (HsRecUpdParent GhcTc)

instance Data (LHsRecUpdFields GhcPs)
instance Data (LHsRecUpdFields GhcRn)
instance Data (LHsRecUpdFields GhcTc)

instance Data (DotFieldOcc GhcPs)
instance Data (DotFieldOcc GhcRn)
instance Data (DotFieldOcc GhcTc)

-- instance (DataIdLR p p) => Data (HsPragE p)
instance Data (HsPragE GhcPs)
instance Data (HsPragE GhcRn)
instance Data (HsPragE GhcTc)

-- instance (DataIdLR p p) => Data (HsExpr p)
instance Data (HsExpr GhcPs)
instance Data (HsExpr GhcRn)
instance Data (HsExpr GhcTc)

-- instance (DataIdLR p p) => Data (HsTupArg p)
instance Data (HsTupArg GhcPs)
instance Data (HsTupArg GhcRn)
instance Data (HsTupArg GhcTc)

-- instance (DataIdLR p p) => Data (HsCmd p)
instance Data (HsCmd GhcPs)
instance Data (HsCmd GhcRn)
instance Data (HsCmd GhcTc)

-- instance (DataIdLR p p) => Data (HsCmdTop p)
instance Data (HsCmdTop GhcPs)
instance Data (HsCmdTop GhcRn)
instance Data (HsCmdTop GhcTc)

-- instance (DataIdLR p p,Data body) => Data (MatchGroup p body)
instance Data (MatchGroup GhcPs (LocatedA (HsExpr GhcPs)))
instance Data (MatchGroup GhcRn (LocatedA (HsExpr GhcRn)))
instance Data (MatchGroup GhcTc (LocatedA (HsExpr GhcTc)))
instance Data (MatchGroup GhcPs (LocatedA (HsCmd GhcPs)))
instance Data (MatchGroup GhcRn (LocatedA (HsCmd GhcRn)))
instance Data (MatchGroup GhcTc (LocatedA (HsCmd GhcTc)))

-- instance (DataIdLR p p,Data body) => Data (Match      p body)
instance Data (Match      GhcPs (LocatedA (HsExpr GhcPs)))
instance Data (Match      GhcRn (LocatedA (HsExpr GhcRn)))
instance Data (Match      GhcTc (LocatedA (HsExpr GhcTc)))
instance Data (Match      GhcPs (LocatedA (HsCmd GhcPs)))
instance Data (Match      GhcRn (LocatedA (HsCmd GhcRn)))
instance Data (Match      GhcTc (LocatedA (HsCmd GhcTc)))

-- instance (DataIdLR p p,Data body) => Data (GRHSs      p body)
instance Data (GRHSs     GhcPs (LocatedA (HsExpr GhcPs)))
instance Data (GRHSs     GhcRn (LocatedA (HsExpr GhcRn)))
instance Data (GRHSs     GhcTc (LocatedA (HsExpr GhcTc)))
instance Data (GRHSs     GhcPs (LocatedA (HsCmd GhcPs)))
instance Data (GRHSs     GhcRn (LocatedA (HsCmd GhcRn)))
instance Data (GRHSs     GhcTc (LocatedA (HsCmd GhcTc)))

-- instance (DataIdLR p p,Data body) => Data (GRHS       p body)
instance Data (GRHS     GhcPs (LocatedA (HsExpr GhcPs)))
instance Data (GRHS     GhcRn (LocatedA (HsExpr GhcRn)))
instance Data (GRHS     GhcTc (LocatedA (HsExpr GhcTc)))
instance Data (GRHS     GhcPs (LocatedA (HsCmd GhcPs)))
instance Data (GRHS     GhcRn (LocatedA (HsCmd GhcRn)))
instance Data (GRHS     GhcTc (LocatedA (HsCmd GhcTc)))

-- instance (DataIdLR p p,Data body) => Data (StmtLR   p p body)
instance Data (StmtLR   GhcPs GhcPs (LocatedA (HsExpr GhcPs)))
instance Data (StmtLR   GhcPs GhcRn (LocatedA (HsExpr GhcRn)))
instance Data (StmtLR   GhcRn GhcRn (LocatedA (HsExpr GhcRn)))
instance Data (StmtLR   GhcTc GhcTc (LocatedA (HsExpr GhcTc)))
instance Data (StmtLR   GhcPs GhcPs (LocatedA (HsCmd GhcPs)))
instance Data (StmtLR   GhcPs GhcRn (LocatedA (HsCmd GhcRn)))
instance Data (StmtLR   GhcRn GhcRn (LocatedA (HsCmd GhcRn)))
instance Data (StmtLR   GhcTc GhcTc (LocatedA (HsCmd GhcTc)))

instance Data RecStmtTc

-- instance (DataIdLR p p) => Data (ParStmtBlock p p)
instance Data (ParStmtBlock GhcPs GhcPs)
instance Data (ParStmtBlock GhcPs GhcRn)
instance Data (ParStmtBlock GhcRn GhcRn)
instance Data (ParStmtBlock GhcTc GhcTc)

-- instance (DataIdLR p p) => Data (ApplicativeStmt p p)
instance Data (ApplicativeStmt GhcPs GhcPs)
instance Data (ApplicativeStmt GhcPs GhcRn)
instance Data (ApplicativeStmt GhcPs GhcTc)
instance Data (ApplicativeStmt GhcRn GhcPs)
instance Data (ApplicativeStmt GhcRn GhcRn)
instance Data (ApplicativeStmt GhcRn GhcTc)
instance Data (ApplicativeStmt GhcTc GhcPs)
instance Data (ApplicativeStmt GhcTc GhcRn)
instance Data (ApplicativeStmt GhcTc GhcTc)

-- instance (DataIdLR p p) => Data (ApplicativeArg p)
instance Data (ApplicativeArg GhcPs)
instance Data (ApplicativeArg GhcRn)
instance Data (ApplicativeArg GhcTc)

instance Data HsArrowMatchContext

instance Data fn => Data (HsStmtContext fn)
instance Data fn => Data (HsMatchContext fn)

-- instance (DataIdLR p p) => Data (HsUntypedSplice p)
instance Data (HsUntypedSplice GhcPs)
instance Data (HsUntypedSplice GhcRn)
instance Data (HsUntypedSplice GhcTc)

instance Data (HsTypedSplice GhcPs)
instance Data (HsTypedSplice GhcRn)
instance Data (HsTypedSplice GhcTc)

instance Data HsImplicitLiftSplice
instance Data HsUserSpliceExt
instance Data HsQuasiQuoteExt

instance Data a => Data (HsUntypedSpliceResult a)
instance Data HsTypedSpliceResult

-- instance (DataIdLR p p) => Data (HsQuote p)
instance Data (HsQuote GhcPs)
instance Data (HsQuote GhcRn)
instance Data (HsQuote GhcTc)

instance Data HsBracketTc

-- instance (DataIdLR p p) => Data (ArithSeqInfo p)
instance Data (ArithSeqInfo GhcPs)
instance Data (ArithSeqInfo GhcRn)
instance Data (ArithSeqInfo GhcTc)

instance Data CmdTopTc
instance Data PendingRnSplice
instance Data PendingTcSplice
instance Data SyntaxExprRn
instance Data SyntaxExprTc

instance Data XBindStmtRn
instance Data XBindStmtTc

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Lit ------------------------------------

-- instance (DataId p) => Data (HsLit p)
instance Data (HsLit GhcPs)
instance Data (HsLit GhcRn)

instance Data HsLitTc
instance Data (HsLit GhcTc)

-- instance (DataIdLR p p) => Data (HsOverLit p)
instance Data (HsOverLit GhcPs)
instance Data (HsOverLit GhcRn)
instance Data (HsOverLit GhcTc)

instance Data OverLitRn
instance Data OverLitTc

instance Data (HsQualLit GhcPs)
instance Data (HsQualLit GhcRn)
instance Data (HsQualLit GhcTc)

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Pat ------------------------------------

-- instance (DataIdLR p p) => Data (Pat p)
instance Data (Pat GhcPs)
instance Data (Pat GhcRn)
instance Data (Pat GhcTc)

instance Data ConPatTc

instance (Data a, Data b) => Data (HsFieldBind a b)

instance (Data body) => Data (HsRecFields GhcPs body)
instance (Data body) => Data (HsRecFields GhcRn body)
instance (Data body) => Data (HsRecFields GhcTc body)

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Type ----------------------------------

-- instance Data (HsModifierOf ty p)
instance Data ModifierPrintsAs
instance Data (HsModifierOf (LocatedA (HsType GhcPs)) GhcPs)
instance Data (HsModifierOf (LocatedA (HsType GhcRn)) GhcRn)
instance Data (HsModifierOf (LocatedA (HsType GhcRn)) GhcTc)
instance Data (HsModifierOf (LocatedA (HsExpr GhcPs)) GhcPs)
instance Data (HsModifierOf (LocatedA (HsExpr GhcRn)) GhcRn)
instance Data (HsModifierOf (LocatedA (HsExpr GhcTc)) GhcTc)

-- instance Data (HsBndrVis p)
instance Data (HsBndrVis GhcPs)
instance Data (HsBndrVis GhcRn)
instance Data (HsBndrVis GhcTc)

-- instance (DataIdLR p p) => Data (LHsQTyVars p)
instance Data (LHsQTyVars GhcPs)
instance Data (LHsQTyVars GhcRn)
instance Data (LHsQTyVars GhcTc)

-- instance (Data flag, DataIdLR p p) => Data (HsOuterTyVarBndrs p)
instance Data flag => Data (HsOuterTyVarBndrs flag GhcPs)
instance Data flag => Data (HsOuterTyVarBndrs flag GhcRn)
instance Data flag => Data (HsOuterTyVarBndrs flag GhcTc)

-- instance (DataIdLR p p) => Data (HsSigType p)
instance Data (HsSigType GhcPs)
instance Data (HsSigType GhcRn)
instance Data (HsSigType GhcTc)

-- instance (DataIdLR p p, Data thing) =>Data (HsWildCardBndrs p thing)
instance (Data thing) => Data (HsWildCardBndrs GhcPs thing)
instance (Data thing) => Data (HsWildCardBndrs GhcRn thing)
instance (Data thing) => Data (HsWildCardBndrs GhcTc thing)

-- instance (DataIdLR p p) => Data (HsPatSigType p)
instance Data (HsPatSigType GhcPs)
instance Data (HsPatSigType GhcRn)
instance Data (HsPatSigType GhcTc)

-- instance (DataIdLR p p) => Data (HsTyPat p)
instance Data (HsTyPat GhcPs)
instance Data (HsTyPat GhcRn)
instance Data (HsTyPat GhcTc)

-- instance (DataIdLR p p) => Data (HsForAllTelescope p)
instance Data (HsForAllTelescope GhcPs)
instance Data (HsForAllTelescope GhcRn)
instance Data (HsForAllTelescope GhcTc)

-- instance (DataIdLR p p) => Data (HsTyVarBndr p)
instance (Data flag) => Data (HsTyVarBndr flag GhcPs)
instance (Data flag) => Data (HsTyVarBndr flag GhcRn)
instance (Data flag) => Data (HsTyVarBndr flag GhcTc)

-- instance Data (HsBndrVar p)
instance Data (HsBndrVar GhcPs)
instance Data (HsBndrVar GhcRn)
instance Data (HsBndrVar GhcTc)

-- instance (DataIdLR p p) => Data (HsBndrKind p)
instance Data (HsBndrKind GhcPs)
instance Data (HsBndrKind GhcRn)
instance Data (HsBndrKind GhcTc)

-- instance (DataIdLR p p) => Data (HsType p)
instance Data (HsType GhcPs)
instance Data (HsType GhcRn)
instance Data (HsType GhcTc)

instance Data HsTypeGhcPsExt

-- instance (DataIdLR p p) => Data (HsFunArr p)
instance Data (HsFunArr GhcPs)
instance Data (HsFunArr GhcRn)
instance Data (HsFunArr GhcTc)

-- instance (Data mult, DataIdLR p p) => Data (HsModifiedFunArrOf mult p)
instance Data (HsModifiedFunArrOf (LocatedA (HsType GhcPs)) GhcPs)
instance Data (HsModifiedFunArrOf (LocatedA (HsType GhcRn)) GhcRn)
instance Data (HsModifiedFunArrOf (LocatedA (HsType GhcRn)) GhcTc)
instance Data (HsModifiedFunArrOf (LocatedA (HsExpr GhcPs)) GhcPs)
instance Data (HsModifiedFunArrOf (LocatedA (HsExpr GhcRn)) GhcRn)
instance Data (HsModifiedFunArrOf (LocatedA (HsExpr GhcTc)) GhcTc)

-- instance (Data a, Data b) => Data (HsArg p a b)
instance (Data a, Data b) => Data (HsArg GhcPs a b)
instance (Data a, Data b) => Data (HsArg GhcRn a b)
instance (Data a, Data b) => Data (HsArg GhcTc a b)

-- instance (DataIdLR p p) => Data (HsConDeclRecField p)
instance Data (HsConDeclRecField GhcPs)
instance Data (HsConDeclRecField GhcRn)
instance Data (HsConDeclRecField GhcTc)

-- instance (DataIdLR p p, Typeable on) => Data (HsConDeclField on p)
instance Data (HsConDeclField GhcPs)
instance Data (HsConDeclField GhcRn)
instance Data (HsConDeclField GhcTc)

-- instance (DataId p)     => Data (FieldOcc p)
instance Data (FieldOcc GhcPs)
instance Data (FieldOcc GhcRn)
instance Data (FieldOcc GhcTc)

-- instance (DataId name) => Data (ImportDecl name)
instance Data (ImportDecl GhcPs)
instance Data (ImportDecl GhcRn)
instance Data (ImportDecl GhcTc)

-- instance Data (IEThingAllExt p)
instance Data (IEThingAllExt GhcPs)
instance Data (IEThingAllExt GhcRn)
instance Data (IEThingAllExt GhcTc)

-- instance Eq (IEThingAllExt p)
instance Eq (IEThingAllExt GhcPs)
instance Eq (IEThingAllExt GhcRn)
instance Eq (IEThingAllExt GhcTc)

-- instance Data (IEWholeNamespaceExt p)
instance Data (IEWholeNamespaceExt GhcPs)
instance Data (IEWholeNamespaceExt GhcRn)
instance Data (IEWholeNamespaceExt GhcTc)

-- instance Eq (IEWholeNamespaceExt p)
instance Eq (IEWholeNamespaceExt GhcPs)
instance Eq (IEWholeNamespaceExt GhcRn)
instance Eq (IEWholeNamespaceExt GhcTc)

-- instance (DataId name)             => Data (IE name)
instance Data (IE GhcPs)
instance Data (IE GhcRn)
instance Data (IE GhcTc)

-- instance (Eq name, Eq (IdP name)) => Eq (IE name)
instance Eq (IE GhcPs)
instance Eq (IE GhcRn)
instance Eq (IE GhcTc)

-- ---------------------------------------------------------------------
instance Data HsCtxt

instance Data XXExprGhcRn

instance Data (HsExpansion GhcRn)
instance Data (HsExpansion GhcTc)

instance Data a => Data (WithUserRdr a)

-- -------------------------------
--------------------------------------
instance Data XXExprGhcTc
instance Data XXPatGhcTc

-- ---------------------------------------------------------------------

instance Data XViaStrategyPs

-- ---------------------------------------------------------------------

instance (Typeable p, Data (Anno (IdGhcP p)), Data (IdGhcP p)) => Data (BooleanFormula (GhcPass p))
---------------------------------------------------------------------

instance Data ActivationGhc

-- instance Data (InlinePragma p)
instance Data (InlinePragma GhcPs)
instance Data (InlinePragma GhcRn)
instance Data (InlinePragma GhcTc)

-- instance Data (OverlapMode p)
instance Data (OverlapMode GhcPs)
instance Data (OverlapMode GhcRn)
instance Data (OverlapMode GhcTc)
