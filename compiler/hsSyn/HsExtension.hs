{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder

module HsExtension where

-- This module captures the type families to precisely identify the extension
-- points for HsSyn

import GhcPrelude

import Data.Data hiding ( Fixity )
import PlaceHolder
import Name
import RdrName
import Var
import Outputable
import SrcLoc (Located)

import Data.Kind

{-
Note [Trees that grow]
~~~~~~~~~~~~~~~~~~~~~~

See https://ghc.haskell.org/trac/ghc/wiki/ImplementingTreesThatGrow

The hsSyn AST is reused across multiple compiler passes. We also have the
Template Haskell AST, and the haskell-src-exts one (outside of GHC)

Supporting multiple passes means the AST has various warts on it to cope with
the specifics for the phases, such as the 'ValBindsOut', 'ConPatOut',
'SigPatOut' etc.

The growable AST will allow each of these variants to be captured explicitly,
such that they only exist in the given compiler pass AST, as selected by the
type parameter to the AST.

In addition it will allow tool writers to define their own extensions to capture
additional information for the tool, in a natural way.

A further goal is to provide a means to harmonise the Template Haskell and
haskell-src-exts ASTs as well.

-}

-- | used as place holder in TTG values
data NoExt = NoExt
  deriving (Data,Eq,Ord)

instance Outputable NoExt where
  ppr _ = text "NoExt"

-- | Used when constructing a term with an unused extension point.
noExt :: NoExt
noExt = NoExt

-- | Used as a data type index for the hsSyn AST
data GhcPass (c :: Pass)
deriving instance Eq (GhcPass c)
deriving instance Typeable c => Data (GhcPass c)

data Pass = Parsed | Renamed | Typechecked
         deriving (Data)

-- Type synonyms as a shorthand for tagging
type GhcPs   = GhcPass 'Parsed      -- Old 'RdrName' type param
type GhcRn   = GhcPass 'Renamed     -- Old 'Name' type param
type GhcTc   = GhcPass 'Typechecked -- Old 'Id' type para,
type GhcTcId = GhcTc                -- Old 'TcId' type param

-- | Maps the "normal" id type for a given pass
type family IdP p
type instance IdP GhcPs = RdrName
type instance IdP GhcRn = Name
type instance IdP GhcTc = Id

type LIdP p = Located (IdP p)

-- | Marks that a field uses the GhcRn variant even when the pass
-- parameter is GhcTc. Useful for storing HsTypes in HsExprs, say, because
-- HsType GhcTc should never occur.
type family NoGhcTc (p :: Type) where
    -- this way, GHC can figure out that the result is a GhcPass
  NoGhcTc (GhcPass pass) = GhcPass (NoGhcTcPass pass)
  NoGhcTc other          = other

type family NoGhcTcPass (p :: Pass) :: Pass where
  NoGhcTcPass 'Typechecked = 'Renamed
  NoGhcTcPass other        = other

-- =====================================================================
-- Type families for the HsBinds extension points

-- HsLocalBindsLR type families
type family XHsValBinds      x x'
type family XHsIPBinds       x x'
type family XEmptyLocalBinds x x'
type family XXHsLocalBindsLR x x'

type ForallXHsLocalBindsLR (c :: * -> Constraint) (x :: *) (x' :: *) =
       ( c (XHsValBinds      x x')
       , c (XHsIPBinds       x x')
       , c (XEmptyLocalBinds x x')
       , c (XXHsLocalBindsLR x x')
       )

-- ValBindsLR type families
type family XValBinds    x x'
type family XXValBindsLR x x'

type ForallXValBindsLR (c :: * -> Constraint) (x :: *) (x' :: *) =
       ( c (XValBinds    x x')
       , c (XXValBindsLR x x')
       )


-- HsBindsLR type families
type family XFunBind    x x'
type family XPatBind    x x'
type family XVarBind    x x'
type family XAbsBinds   x x'
type family XPatSynBind x x'
type family XXHsBindsLR x x'

type ForallXHsBindsLR (c :: * -> Constraint) (x :: *) (x' :: *) =
       ( c (XFunBind    x x')
       , c (XPatBind    x x')
       , c (XVarBind    x x')
       , c (XAbsBinds   x x')
       , c (XPatSynBind x x')
       , c (XXHsBindsLR x x')
       )

-- ABExport type families
type family XABE x
type family XXABExport x

type ForallXABExport (c :: * -> Constraint) (x :: *) =
       ( c (XABE       x)
       , c (XXABExport x)
       )

-- PatSynBind type families
type family XPSB x x'
type family XXPatSynBind x x'

type ForallXPatSynBind  (c :: * -> Constraint) (x :: *) (x' :: *) =
       ( c (XPSB         x x')
       , c (XXPatSynBind x x')
       )

-- HsIPBinds type families
type family XIPBinds    x
type family XXHsIPBinds x

type ForallXHsIPBinds (c :: * -> Constraint) (x :: *) =
       ( c (XIPBinds    x)
       , c (XXHsIPBinds x)
       )

-- IPBind type families
type family XCIPBind x
type family XXIPBind x

type ForallXIPBind (c :: * -> Constraint) (x :: *) =
       ( c (XCIPBind x)
       , c (XXIPBind x)
       )

-- Sig type families
type family XTypeSig          x
type family XPatSynSig        x
type family XClassOpSig       x
type family XIdSig            x
type family XFixSig           x
type family XInlineSig        x
type family XSpecSig          x
type family XSpecInstSig      x
type family XMinimalSig       x
type family XSCCFunSig        x
type family XCompleteMatchSig x
type family XXSig             x

type ForallXSig (c :: * -> Constraint) (x :: *) =
       ( c (XTypeSig          x)
       , c (XPatSynSig        x)
       , c (XClassOpSig       x)
       , c (XIdSig            x)
       , c (XFixSig           x)
       , c (XInlineSig        x)
       , c (XSpecSig          x)
       , c (XSpecInstSig      x)
       , c (XMinimalSig       x)
       , c (XSCCFunSig        x)
       , c (XCompleteMatchSig x)
       , c (XXSig             x)
       )

-- FixitySig type families
type family XFixitySig          x
type family XXFixitySig         x

type ForallXFixitySig (c :: * -> Constraint) (x :: *) =
       ( c (XFixitySig         x)
       , c (XXFixitySig        x)
       )

-- =====================================================================
-- Type families for the HsDecls extension points

-- HsDecl type families
type family XTyClD       x
type family XInstD       x
type family XDerivD      x
type family XValD        x
type family XSigD        x
type family XDefD        x
type family XForD        x
type family XWarningD    x
type family XAnnD        x
type family XRuleD       x
type family XSpliceD     x
type family XDocD        x
type family XRoleAnnotD  x
type family XXHsDecl     x

type ForallXHsDecl (c :: * -> Constraint) (x :: *) =
       ( c (XTyClD       x)
       , c (XInstD       x)
       , c (XDerivD      x)
       , c (XValD        x)
       , c (XSigD        x)
       , c (XDefD        x)
       , c (XForD        x)
       , c (XWarningD    x)
       , c (XAnnD        x)
       , c (XRuleD       x)
       , c (XSpliceD     x)
       , c (XDocD        x)
       , c (XRoleAnnotD  x)
       , c (XXHsDecl    x)
       )

-- -------------------------------------
-- HsGroup type families
type family XCHsGroup      x
type family XXHsGroup      x

type ForallXHsGroup (c :: * -> Constraint) (x :: *) =
       ( c (XCHsGroup       x)
       , c (XXHsGroup       x)
       )

-- -------------------------------------
-- SpliceDecl type families
type family XSpliceDecl       x
type family XXSpliceDecl      x

type ForallXSpliceDecl (c :: * -> Constraint) (x :: *) =
       ( c (XSpliceDecl        x)
       , c (XXSpliceDecl       x)
       )

-- -------------------------------------
-- TyClDecl type families
type family XFamDecl       x
type family XSynDecl       x
type family XDataDecl      x
type family XClassDecl     x
type family XXTyClDecl     x

type ForallXTyClDecl (c :: * -> Constraint) (x :: *) =
       ( c (XFamDecl       x)
       , c (XSynDecl       x)
       , c (XDataDecl      x)
       , c (XClassDecl     x)
       , c (XXTyClDecl     x)
       )

-- -------------------------------------
-- TyClGroup type families
type family XCTyClGroup      x
type family XXTyClGroup      x

type ForallXTyClGroup (c :: * -> Constraint) (x :: *) =
       ( c (XCTyClGroup       x)
       , c (XXTyClGroup       x)
       )

-- -------------------------------------
-- FamilyResultSig type families
type family XNoSig            x
type family XCKindSig         x -- Clashes with XKindSig above
type family XTyVarSig         x
type family XXFamilyResultSig x

type ForallXFamilyResultSig (c :: * -> Constraint) (x :: *) =
       ( c (XNoSig            x)
       , c (XCKindSig         x)
       , c (XTyVarSig         x)
       , c (XXFamilyResultSig x)
       )

-- -------------------------------------
-- FamilyDecl type families
type family XCFamilyDecl      x
type family XXFamilyDecl      x

type ForallXFamilyDecl (c :: * -> Constraint) (x :: *) =
       ( c (XCFamilyDecl       x)
       , c (XXFamilyDecl       x)
       )

-- -------------------------------------
-- HsDataDefn type families
type family XCHsDataDefn      x
type family XXHsDataDefn      x

type ForallXHsDataDefn (c :: * -> Constraint) (x :: *) =
       ( c (XCHsDataDefn       x)
       , c (XXHsDataDefn       x)
       )

-- -------------------------------------
-- HsDerivingClause type families
type family XCHsDerivingClause      x
type family XXHsDerivingClause      x

type ForallXHsDerivingClause (c :: * -> Constraint) (x :: *) =
       ( c (XCHsDerivingClause       x)
       , c (XXHsDerivingClause       x)
       )

-- -------------------------------------
-- ConDecl type families
type family XConDeclGADT   x
type family XConDeclH98    x
type family XXConDecl      x

type ForallXConDecl (c :: * -> Constraint) (x :: *) =
       ( c (XConDeclGADT    x)
       , c (XConDeclH98     x)
       , c (XXConDecl       x)
       )

-- -------------------------------------
-- FamEqn type families
type family XCFamEqn      x p r
type family XXFamEqn      x p r

type ForallXFamEqn (c :: * -> Constraint) (x :: *) (p :: *) (r :: *) =
       ( c (XCFamEqn       x p r)
       , c (XXFamEqn       x p r)
       )

-- -------------------------------------
-- ClsInstDecl type families
type family XCClsInstDecl      x
type family XXClsInstDecl      x

type ForallXClsInstDecl (c :: * -> Constraint) (x :: *) =
       ( c (XCClsInstDecl       x)
       , c (XXClsInstDecl       x)
       )

-- -------------------------------------
-- ClsInstDecl type families
type family XClsInstD      x
type family XDataFamInstD  x
type family XTyFamInstD    x
type family XXInstDecl     x

type ForallXInstDecl (c :: * -> Constraint) (x :: *) =
       ( c (XClsInstD       x)
       , c (XDataFamInstD   x)
       , c (XTyFamInstD     x)
       , c (XXInstDecl      x)
       )

-- -------------------------------------
-- DerivDecl type families
type family XCDerivDecl      x
type family XXDerivDecl      x

type ForallXDerivDecl (c :: * -> Constraint) (x :: *) =
       ( c (XCDerivDecl       x)
       , c (XXDerivDecl       x)
       )

-- -------------------------------------
-- DerivStrategy type family
type family XViaStrategy x

-- -------------------------------------
-- DefaultDecl type families
type family XCDefaultDecl      x
type family XXDefaultDecl      x

type ForallXDefaultDecl (c :: * -> Constraint) (x :: *) =
       ( c (XCDefaultDecl       x)
       , c (XXDefaultDecl       x)
       )

-- -------------------------------------
-- DefaultDecl type families
type family XForeignImport     x
type family XForeignExport     x
type family XXForeignDecl      x

type ForallXForeignDecl (c :: * -> Constraint) (x :: *) =
       ( c (XForeignImport      x)
       , c (XForeignExport      x)
       , c (XXForeignDecl       x)
       )

-- -------------------------------------
-- RuleDecls type families
type family XCRuleDecls      x
type family XXRuleDecls      x

type ForallXRuleDecls (c :: * -> Constraint) (x :: *) =
       ( c (XCRuleDecls       x)
       , c (XXRuleDecls       x)
       )


-- -------------------------------------
-- RuleDecl type families
type family XHsRule          x
type family XXRuleDecl       x

type ForallXRuleDecl (c :: * -> Constraint) (x :: *) =
       ( c (XHsRule           x)
       , c (XXRuleDecl        x)
       )

-- -------------------------------------
-- RuleBndr type families
type family XCRuleBndr      x
type family XRuleBndrSig    x
type family XXRuleBndr      x

type ForallXRuleBndr (c :: * -> Constraint) (x :: *) =
       ( c (XCRuleBndr       x)
       , c (XRuleBndrSig     x)
       , c (XXRuleBndr       x)
       )

-- -------------------------------------
-- WarnDecls type families
type family XWarnings        x
type family XXWarnDecls      x

type ForallXWarnDecls (c :: * -> Constraint) (x :: *) =
       ( c (XWarnings        x)
       , c (XXWarnDecls      x)
       )

-- -------------------------------------
-- AnnDecl type families
type family XWarning        x
type family XXWarnDecl      x

type ForallXWarnDecl (c :: * -> Constraint) (x :: *) =
       ( c (XWarning        x)
       , c (XXWarnDecl      x)
       )

-- -------------------------------------
-- AnnDecl type families
type family XHsAnnotation  x
type family XXAnnDecl      x

type ForallXAnnDecl (c :: * -> Constraint) (x :: *) =
       ( c (XHsAnnotation  x)
       , c (XXAnnDecl      x)
       )

-- -------------------------------------
-- RoleAnnotDecl type families
type family XCRoleAnnotDecl  x
type family XXRoleAnnotDecl  x

type ForallXRoleAnnotDecl (c :: * -> Constraint) (x :: *) =
       ( c (XCRoleAnnotDecl  x)
       , c (XXRoleAnnotDecl  x)
       )

-- =====================================================================
-- Type families for the HsExpr extension points

type family XVar            x
type family XUnboundVar     x
type family XConLikeOut     x
type family XRecFld         x
type family XOverLabel      x
type family XIPVar          x
type family XOverLitE       x
type family XLitE           x
type family XLam            x
type family XLamCase        x
type family XApp            x
type family XAppTypeE       x
type family XOpApp          x
type family XNegApp         x
type family XPar            x
type family XSectionL       x
type family XSectionR       x
type family XExplicitTuple  x
type family XExplicitSum    x
type family XCase           x
type family XIf             x
type family XMultiIf        x
type family XLet            x
type family XDo             x
type family XExplicitList   x
type family XRecordCon      x
type family XRecordUpd      x
type family XExprWithTySig  x
type family XArithSeq       x
type family XSCC            x
type family XCoreAnn        x
type family XBracket        x
type family XRnBracketOut   x
type family XTcBracketOut   x
type family XSpliceE        x
type family XProc           x
type family XStatic         x
type family XArrApp         x
type family XArrForm        x
type family XTick           x
type family XBinTick        x
type family XTickPragma     x
type family XEWildPat       x
type family XEAsPat         x
type family XEViewPat       x
type family XELazyPat       x
type family XWrap           x
type family XXExpr          x

type ForallXExpr (c :: * -> Constraint) (x :: *) =
       ( c (XVar            x)
       , c (XUnboundVar     x)
       , c (XConLikeOut     x)
       , c (XRecFld         x)
       , c (XOverLabel      x)
       , c (XIPVar          x)
       , c (XOverLitE       x)
       , c (XLitE           x)
       , c (XLam            x)
       , c (XLamCase        x)
       , c (XApp            x)
       , c (XAppTypeE       x)
       , c (XOpApp          x)
       , c (XNegApp         x)
       , c (XPar            x)
       , c (XSectionL       x)
       , c (XSectionR       x)
       , c (XExplicitTuple  x)
       , c (XExplicitSum    x)
       , c (XCase           x)
       , c (XIf             x)
       , c (XMultiIf        x)
       , c (XLet            x)
       , c (XDo             x)
       , c (XExplicitList   x)
       , c (XRecordCon      x)
       , c (XRecordUpd      x)
       , c (XExprWithTySig  x)
       , c (XArithSeq       x)
       , c (XSCC            x)
       , c (XCoreAnn        x)
       , c (XBracket        x)
       , c (XRnBracketOut   x)
       , c (XTcBracketOut   x)
       , c (XSpliceE        x)
       , c (XProc           x)
       , c (XStatic         x)
       , c (XArrApp         x)
       , c (XArrForm        x)
       , c (XTick           x)
       , c (XBinTick        x)
       , c (XTickPragma     x)
       , c (XEWildPat       x)
       , c (XEAsPat         x)
       , c (XEViewPat       x)
       , c (XELazyPat       x)
       , c (XWrap           x)
       , c (XXExpr          x)
       )
-- ---------------------------------------------------------------------

type family XUnambiguous        x
type family XAmbiguous          x
type family XXAmbiguousFieldOcc x

type ForallXAmbiguousFieldOcc (c :: * -> Constraint) (x :: *) =
       ( c (XUnambiguous        x)
       , c (XAmbiguous          x)
       , c (XXAmbiguousFieldOcc x)
       )

-- ----------------------------------------------------------------------

type family XPresent  x
type family XMissing  x
type family XXTupArg  x

type ForallXTupArg (c :: * -> Constraint) (x :: *) =
       ( c (XPresent x)
       , c (XMissing x)
       , c (XXTupArg x)
       )

-- ---------------------------------------------------------------------

type family XTypedSplice   x
type family XUntypedSplice x
type family XQuasiQuote    x
type family XSpliced       x
type family XXSplice       x

type ForallXSplice (c :: * -> Constraint) (x :: *) =
       ( c (XTypedSplice   x)
       , c (XUntypedSplice x)
       , c (XQuasiQuote    x)
       , c (XSpliced       x)
       , c (XXSplice       x)
       )

-- ---------------------------------------------------------------------

type family XExpBr      x
type family XPatBr      x
type family XDecBrL     x
type family XDecBrG     x
type family XTypBr      x
type family XVarBr      x
type family XTExpBr     x
type family XXBracket   x

type ForallXBracket (c :: * -> Constraint) (x :: *) =
       ( c (XExpBr      x)
       , c (XPatBr      x)
       , c (XDecBrL     x)
       , c (XDecBrG     x)
       , c (XTypBr      x)
       , c (XVarBr      x)
       , c (XTExpBr     x)
       , c (XXBracket   x)
       )

-- ---------------------------------------------------------------------

type family XCmdTop  x
type family XXCmdTop x

type ForallXCmdTop (c :: * -> Constraint) (x :: *) =
       ( c (XCmdTop  x)
       , c (XXCmdTop x)
       )

-- -------------------------------------

type family XMG           x b
type family XXMatchGroup  x b

type ForallXMatchGroup (c :: * -> Constraint) (x :: *) (b :: *) =
       ( c (XMG          x b)
       , c (XXMatchGroup x b)
       )

-- -------------------------------------

type family XCMatch  x b
type family XXMatch  x b

type ForallXMatch (c :: * -> Constraint) (x :: *) (b :: *) =
       ( c (XCMatch  x b)
       , c (XXMatch  x b)
       )

-- -------------------------------------

type family XCGRHSs  x b
type family XXGRHSs  x b

type ForallXGRHSs (c :: * -> Constraint) (x :: *) (b :: *) =
       ( c (XCGRHSs  x b)
       , c (XXGRHSs  x b)
       )

-- -------------------------------------

type family XCGRHS  x b
type family XXGRHS  x b

type ForallXGRHS (c :: * -> Constraint) (x :: *) (b :: *) =
       ( c (XCGRHS  x b)
       , c (XXGRHS  x b)
       )

-- -------------------------------------

type family XLastStmt        x x' b
type family XBindStmt        x x' b
type family XApplicativeStmt x x' b
type family XBodyStmt        x x' b
type family XLetStmt         x x' b
type family XParStmt         x x' b
type family XTransStmt       x x' b
type family XRecStmt         x x' b
type family XXStmtLR         x x' b

type ForallXStmtLR (c :: * -> Constraint) (x :: *)  (x' :: *) (b :: *) =
       ( c (XLastStmt         x x' b)
       , c (XBindStmt         x x' b)
       , c (XApplicativeStmt  x x' b)
       , c (XBodyStmt         x x' b)
       , c (XLetStmt          x x' b)
       , c (XParStmt          x x' b)
       , c (XTransStmt        x x' b)
       , c (XRecStmt          x x' b)
       , c (XXStmtLR          x x' b)
       )

-- ---------------------------------------------------------------------

type family XCmdArrApp  x
type family XCmdArrForm x
type family XCmdApp     x
type family XCmdLam     x
type family XCmdPar     x
type family XCmdCase    x
type family XCmdIf      x
type family XCmdLet     x
type family XCmdDo      x
type family XCmdWrap    x
type family XXCmd       x

type ForallXCmd (c :: * -> Constraint) (x :: *) =
       ( c (XCmdArrApp  x)
       , c (XCmdArrForm x)
       , c (XCmdApp     x)
       , c (XCmdLam     x)
       , c (XCmdPar     x)
       , c (XCmdCase    x)
       , c (XCmdIf      x)
       , c (XCmdLet     x)
       , c (XCmdDo      x)
       , c (XCmdWrap    x)
       , c (XXCmd       x)
       )

-- ---------------------------------------------------------------------

type family XParStmtBlock  x x'
type family XXParStmtBlock x x'

type ForallXParStmtBlock (c :: * -> Constraint) (x :: *) (x' :: *) =
       ( c (XParStmtBlock  x x')
       , c (XXParStmtBlock x x')
       )

-- ---------------------------------------------------------------------

type family XApplicativeArgOne   x
type family XApplicativeArgMany  x
type family XXApplicativeArg     x

type ForallXApplicativeArg (c :: * -> Constraint) (x :: *) =
       ( c (XApplicativeArgOne   x)
       , c (XApplicativeArgMany  x)
       , c (XXApplicativeArg     x)
       )

-- =====================================================================
-- Type families for the HsImpExp extension points

-- TODO

-- =====================================================================
-- Type families for the HsLit extension points

-- We define a type family for each extension point. This is based on prepending
-- 'X' to the constructor name, for ease of reference.
type family XHsChar x
type family XHsCharPrim x
type family XHsString x
type family XHsStringPrim x
type family XHsInt x
type family XHsIntPrim x
type family XHsWordPrim x
type family XHsInt64Prim x
type family XHsWord64Prim x
type family XHsInteger x
type family XHsRat x
type family XHsFloatPrim x
type family XHsDoublePrim x
type family XXLit x

-- | Helper to apply a constraint to all extension points. It has one
-- entry per extension point type family.
type ForallXHsLit (c :: * -> Constraint) (x :: *) =
  ( c (XHsChar       x)
  , c (XHsCharPrim   x)
  , c (XHsDoublePrim x)
  , c (XHsFloatPrim  x)
  , c (XHsInt        x)
  , c (XHsInt64Prim  x)
  , c (XHsIntPrim    x)
  , c (XHsInteger    x)
  , c (XHsRat        x)
  , c (XHsString     x)
  , c (XHsStringPrim x)
  , c (XHsWord64Prim x)
  , c (XHsWordPrim   x)
  , c (XXLit         x)
  )

type family XOverLit  x
type family XXOverLit x

type ForallXOverLit (c :: * -> Constraint) (x :: *) =
       ( c (XOverLit  x)
       , c (XXOverLit x)
       )

-- =====================================================================
-- Type families for the HsPat extension points

type family XWildPat   x
type family XVarPat    x
type family XLazyPat   x
type family XAsPat     x
type family XParPat    x
type family XBangPat   x
type family XListPat   x
type family XTuplePat  x
type family XSumPat    x
type family XConPat    x
type family XViewPat   x
type family XSplicePat x
type family XLitPat    x
type family XNPat      x
type family XNPlusKPat x
type family XSigPat    x
type family XCoPat     x
type family XXPat      x


type ForallXPat (c :: * -> Constraint) (x :: *) =
       ( c (XWildPat   x)
       , c (XVarPat    x)
       , c (XLazyPat   x)
       , c (XAsPat     x)
       , c (XParPat    x)
       , c (XBangPat   x)
       , c (XListPat   x)
       , c (XTuplePat  x)
       , c (XSumPat    x)
       , c (XViewPat   x)
       , c (XSplicePat x)
       , c (XLitPat    x)
       , c (XNPat      x)
       , c (XNPlusKPat x)
       , c (XSigPat    x)
       , c (XCoPat     x)
       , c (XXPat      x)
       )

-- =====================================================================
-- Type families for the HsTypes type families

type family XHsQTvs       x
type family XXLHsQTyVars  x

type ForallXLHsQTyVars (c :: * -> Constraint) (x :: *) =
       ( c (XHsQTvs       x)
       , c (XXLHsQTyVars  x)
       )

-- -------------------------------------

type family XHsIB              x b
type family XXHsImplicitBndrs  x b

type ForallXHsImplicitBndrs (c :: * -> Constraint) (x :: *) (b :: *) =
       ( c (XHsIB              x b)
       , c (XXHsImplicitBndrs  x b)
       )

-- -------------------------------------

type family XHsWC              x b
type family XXHsWildCardBndrs  x b

type ForallXHsWildCardBndrs(c :: * -> Constraint) (x :: *) (b :: *) =
       ( c (XHsWC              x b)
       , c (XXHsWildCardBndrs  x b)
       )

-- -------------------------------------

type family XForAllTy        x
type family XQualTy          x
type family XTyVar           x
type family XAppTy           x
type family XAppKindTy       x
type family XFunTy           x
type family XListTy          x
type family XTupleTy         x
type family XSumTy           x
type family XOpTy            x
type family XParTy           x
type family XIParamTy        x
type family XStarTy          x
type family XKindSig         x
type family XSpliceTy        x
type family XDocTy           x
type family XBangTy          x
type family XRecTy           x
type family XExplicitListTy  x
type family XExplicitTupleTy x
type family XTyLit           x
type family XWildCardTy      x
type family XXType           x

-- | Helper to apply a constraint to all extension points. It has one
-- entry per extension point type family.
type ForallXType (c :: * -> Constraint) (x :: *) =
       ( c (XForAllTy        x)
       , c (XQualTy          x)
       , c (XTyVar           x)
       , c (XAppTy           x)
       , c (XAppKindTy       x)
       , c (XFunTy           x)
       , c (XListTy          x)
       , c (XTupleTy         x)
       , c (XSumTy           x)
       , c (XOpTy            x)
       , c (XParTy           x)
       , c (XIParamTy        x)
       , c (XStarTy          x)
       , c (XKindSig         x)
       , c (XSpliceTy        x)
       , c (XDocTy           x)
       , c (XBangTy          x)
       , c (XRecTy           x)
       , c (XExplicitListTy  x)
       , c (XExplicitTupleTy x)
       , c (XTyLit           x)
       , c (XWildCardTy      x)
       , c (XXType           x)
       )

-- ---------------------------------------------------------------------

type family XUserTyVar   x
type family XKindedTyVar x
type family XXTyVarBndr  x

type ForallXTyVarBndr (c :: * -> Constraint) (x :: *) =
       ( c (XUserTyVar      x)
       , c (XKindedTyVar    x)
       , c (XXTyVarBndr     x)
       )

-- ---------------------------------------------------------------------

type family XConDeclField  x
type family XXConDeclField x

type ForallXConDeclField (c :: * -> Constraint) (x :: *) =
       ( c (XConDeclField  x)
       , c (XXConDeclField x)
       )

-- ---------------------------------------------------------------------

type family XCFieldOcc x
type family XXFieldOcc x

type ForallXFieldOcc (c :: * -> Constraint) (x :: *) =
       ( c (XCFieldOcc x)
       , c (XXFieldOcc x)
       )


-- =====================================================================
-- Type families for the HsImpExp type families

type family XCImportDecl       x
type family XXImportDecl       x

type ForallXImportDecl (c :: * -> Constraint) (x :: *) =
       ( c (XCImportDecl x)
       , c (XXImportDecl x)
       )

-- -------------------------------------

type family XIEVar             x
type family XIEThingAbs        x
type family XIEThingAll        x
type family XIEThingWith       x
type family XIEModuleContents  x
type family XIEGroup           x
type family XIEDoc             x
type family XIEDocNamed        x
type family XXIE               x

type ForallXIE (c :: * -> Constraint) (x :: *) =
       ( c (XIEVar x)
       , c (XIEThingAbs        x)
       , c (XIEThingAll        x)
       , c (XIEThingWith       x)
       , c (XIEModuleContents  x)
       , c (XIEGroup           x)
       , c (XIEDoc             x)
       , c (XIEDocNamed        x)
       , c (XXIE               x)
       )

-- -------------------------------------


-- =====================================================================
-- End of Type family definitions
-- =====================================================================

-- ----------------------------------------------------------------------
-- | Conversion of annotations from one type index to another. This is required
-- where the AST is converted from one pass to another, and the extension values
-- need to be brought along if possible. So for example a 'SourceText' is
-- converted via 'id', but needs a type signature to keep the type checker
-- happy.
class Convertable a b  | a -> b where
  convert :: a -> b

instance Convertable a a where
  convert = id

-- | A constraint capturing all the extension points that can be converted via
-- @instance Convertable a a@
type ConvertIdX a b =
  (XHsDoublePrim a ~ XHsDoublePrim b,
   XHsFloatPrim a ~ XHsFloatPrim b,
   XHsRat a ~ XHsRat b,
   XHsInteger a ~ XHsInteger b,
   XHsWord64Prim a ~ XHsWord64Prim b,
   XHsInt64Prim a ~ XHsInt64Prim b,
   XHsWordPrim a ~ XHsWordPrim b,
   XHsIntPrim a ~ XHsIntPrim b,
   XHsInt a ~ XHsInt b,
   XHsStringPrim a ~ XHsStringPrim b,
   XHsString a ~ XHsString b,
   XHsCharPrim a ~ XHsCharPrim b,
   XHsChar a ~ XHsChar b,
   XXLit a ~ XXLit b)

-- ----------------------------------------------------------------------

-- Note [OutputableX]
-- ~~~~~~~~~~~~~~~~~~
--
-- is required because the type family resolution
-- process cannot determine that all cases are handled for a `GhcPass p`
-- case where the cases are listed separately.
--
-- So
--
--   type instance XXHsIPBinds    (GhcPass p) = NoExt
--
-- will correctly deduce Outputable for (GhcPass p), but
--
--   type instance XIPBinds       GhcPs = NoExt
--   type instance XIPBinds       GhcRn = NoExt
--   type instance XIPBinds       GhcTc = TcEvBinds
--
-- will not.


-- | Provide a summary constraint that gives all am Outputable constraint to
-- extension points needing one
type OutputableX p = -- See Note [OutputableX]
  ( Outputable (XIPBinds    p)
  , Outputable (XViaStrategy p)
  , Outputable (XViaStrategy GhcRn)
  )
-- TODO: Should OutputableX be included in OutputableBndrId?

-- ----------------------------------------------------------------------

-- |Constraint type to bundle up the requirement for 'OutputableBndr' on both
-- the @id@ and the 'NameOrRdrName' type for it
type OutputableBndrId id =
  ( OutputableBndr (NameOrRdrName (IdP id))
  , OutputableBndr (IdP id)
  , OutputableBndr (NameOrRdrName (IdP (NoGhcTc id)))
  , OutputableBndr (IdP (NoGhcTc id))
  , NoGhcTc id ~ NoGhcTc (NoGhcTc id)
  , OutputableX id
  , OutputableX (NoGhcTc id)
  )
