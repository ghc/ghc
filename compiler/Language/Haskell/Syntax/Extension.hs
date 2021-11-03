{-# LANGUAGE AllowAmbiguousTypes     #-} -- for unXRec, etc.
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveDataTypeable      #-}
{-# LANGUAGE EmptyCase               #-}
{-# LANGUAGE EmptyDataDeriving       #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE UndecidableInstances    #-} -- Wrinkle in Note [Trees That Grow]
                                         -- in module Language.Haskell.Syntax.Extension

-- See Note [Language.Haskell.Syntax.* Hierarchy] for why not GHC.Hs.*
module Language.Haskell.Syntax.Extension where

-- This module captures the type families to precisely identify the extension
-- points for GHC.Hs syntax

import GHC.Prelude

import GHC.TypeLits (Symbol, KnownSymbol)
import Data.Data hiding ( Fixity )
import Data.Kind (Type)
import GHC.Utils.Outputable

{-
Note [Trees That Grow]
~~~~~~~~~~~~~~~~~~~~~~

See https://gitlab.haskell.org/ghc/ghc/wikis/implementing-trees-that-grow

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

Wrinkle: In order to print out the AST, we need to know it is Outputable.
We also sometimes need to branch on the particular pass that we're in
(e.g. to print out type information once we know it). In order to allow
both of these actions, we define OutputableBndrId, which gathers the necessary
OutputableBndr and IsPass constraints. The use of this constraint in instances
generally requires UndecidableInstances.

See also Note [IsPass] and Note [NoGhcTc] in GHC.Hs.Extension.

-}

-- | A placeholder type for TTG extension points that are not currently
-- unused to represent any particular value.
--
-- This should not be confused with 'DataConCantHappen', which are found in unused
-- extension /constructors/ and therefore should never be inhabited. In
-- contrast, 'NoExtField' is used in extension /points/ (e.g., as the field of
-- some constructor), so it must have an inhabitant to construct AST passes
-- that manipulate fields with that extension point as their type.
data NoExtField = NoExtField
  deriving (Data,Eq,Ord)

instance Outputable NoExtField where
  ppr _ = text "NoExtField"

-- | Used when constructing a term with an unused extension point.
noExtField :: NoExtField
noExtField = NoExtField

{-
Note [Constructor cannot occur]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some data constructors can't occur in certain phases; e.g. the output
of the type checker never has OverLabel. We signal this by
* setting the extension field to DataConCantHappen
* using dataConCantHappen in the cases that can't happen

For example:

   type instance XOverLabel GhcTc = DataConCantHappen

   dsExpr :: HsExpr GhcTc -> blah
   dsExpr (HsOverLabel x _) = dataConCantHappen x

The function dataConCantHappen is defined thus:
   dataConCantHappen :: DataConCantHappen -> a
   dataConCantHappen x = case x of {}
(i.e. identically to Data.Void.absurd, but more helpfully named).
Remember DataConCantHappen is a type whose only element is bottom.

This should not be confused with 'NoExtField', which are found in unused
extension /points/ (not /constructors/) and therefore can be inhabited.

It would be better to omit the pattern match altogether, but we
can only do that if the extension field was strict (#18764).
See also [DataConCantHappen and strict fields].
-}
data DataConCantHappen
  deriving (Data,Eq,Ord)

instance Outputable DataConCantHappen where
  ppr = dataConCantHappen

-- | Eliminate a 'DataConCantHappen'. See Note [Constructor cannot happen].
dataConCantHappen :: DataConCantHappen -> a
dataConCantHappen x = case x of {}

-- | GHC's L prefixed variants wrap their vanilla variant in this type family,
-- to add 'SrcLoc' info via 'Located'. Other passes than 'GhcPass' not
-- interested in location information can define this as
-- @type instance XRec NoLocated a = a@.
-- See Note [XRec and SrcSpans in the AST]
type family XRec p a = r | r -> a

type family Anno a = b -- See Note [XRec and Anno in the AST] in GHC.Parser.Annotation

{-
Note [XRec and SrcSpans in the AST]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
XRec is meant to replace most of the uses of `Located` in the AST. It is another
extension point meant to make it easier for non-GHC applications to reuse the
AST for their own purposes, and not have to deal the hassle of (perhaps) useless
SrcSpans everywhere.

instead of `Located (HsExpr p)` or similar types, we will now have `XRec p
(HsExpr p)`

XRec allows annotating certain points in the AST with extra
information. This maybe be source spans (for GHC), nothing (for TH),
types (for HIE files), exact print annotations (for exactprint) or
anything else.

This should hopefully bring us one step closer to sharing the AST between GHC
and TH.

We use the `UnXRec`, `MapXRec` and `WrapXRec` type classes to aid us in writing
pass-polymorphic code that deals with `XRec`s
-}

-- | We can strip off the XRec to access the underlying data.
-- See Note [XRec and SrcSpans in the AST]
class UnXRec p where
  unXRec :: XRec p a -> a

-- | We can map over the underlying type contained in an @XRec@ while preserving
-- the annotation as is.
class MapXRec p where
  mapXRec :: (Anno a ~ Anno b) => (a -> b) -> XRec p a -> XRec p b
-- See Note [XRec and SrcSpans in the AST]
-- See Note [XRec and Anno in the AST] in GHC.Parser.Annotation
-- AZ: Is there a way to not have Anno in this file, but still have MapXRec?
--     Perhaps define XRec with an additional b parameter, only used in Hs as (Anno b)?

-- | The trivial wrapper that carries no additional information
-- See Note [XRec and SrcSpans in the AST]
class WrapXRec p a where
  wrapXRec :: a -> XRec p a

-- | Maps the "normal" id type for a given pass
type family IdP p

type LIdP p = XRec p (IdP p)

-- =====================================================================
-- Type families for the HsBinds extension points

-- HsLocalBindsLR type families
type family XHsValBinds      x x'
type family XHsIPBinds       x x'
type family XEmptyLocalBinds x x'
type family XXHsLocalBindsLR x x'

-- HsValBindsLR type families
type family XValBinds    x x'
type family XXValBindsLR x x'

-- HsBindLR type families
type family XFunBind    x x'
type family XPatBind    x x'
type family XVarBind    x x'
type family XAbsBinds   x x'
type family XPatSynBind x x'
type family XXHsBindsLR x x'

-- ABExport type families
type family XABE x
type family XXABExport x

-- PatSynBind type families
type family XPSB x x'
type family XXPatSynBind x x'

-- HsIPBinds type families
type family XIPBinds    x
type family XXHsIPBinds x

-- IPBind type families
type family XCIPBind x
type family XXIPBind x

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

-- FixitySig type families
type family XFixitySig          x
type family XXFixitySig         x

-- StandaloneKindSig type families
type family XStandaloneKindSig  x
type family XXStandaloneKindSig x

-- =====================================================================
-- Type families for the HsDecls extension points

-- HsDecl type families
type family XTyClD       x
type family XInstD       x
type family XDerivD      x
type family XValD        x
type family XSigD        x
type family XKindSigD    x
type family XDefD        x
type family XForD        x
type family XWarningD    x
type family XAnnD        x
type family XRuleD       x
type family XSpliceD     x
type family XDocD        x
type family XRoleAnnotD  x
type family XXHsDecl     x

-- -------------------------------------
-- HsGroup type families
type family XCHsGroup      x
type family XXHsGroup      x

-- -------------------------------------
-- SpliceDecl type families
type family XSpliceDecl       x
type family XXSpliceDecl      x

-- -------------------------------------
-- TyClDecl type families
type family XFamDecl       x
type family XSynDecl       x
type family XDataDecl      x
type family XClassDecl     x
type family XXTyClDecl     x

-- -------------------------------------
-- FunDep type families
type family XCFunDep      x
type family XXFunDep      x

-- -------------------------------------
-- TyClGroup type families
type family XCTyClGroup      x
type family XXTyClGroup      x

-- -------------------------------------
-- FamilyResultSig type families
type family XNoSig            x
type family XCKindSig         x -- Clashes with XKindSig above
type family XTyVarSig         x
type family XXFamilyResultSig x

-- -------------------------------------
-- FamilyDecl type families
type family XCFamilyDecl      x
type family XXFamilyDecl      x

-- -------------------------------------
-- HsDataDefn type families
type family XCHsDataDefn      x
type family XXHsDataDefn      x

-- -------------------------------------
-- HsDerivingClause type families
type family XCHsDerivingClause      x
type family XXHsDerivingClause      x

-- -------------------------------------
-- DerivClauseTys type families
type family XDctSingle       x
type family XDctMulti        x
type family XXDerivClauseTys x

-- -------------------------------------
-- ConDecl type families
type family XConDeclGADT   x
type family XConDeclH98    x
type family XXConDecl      x

-- -------------------------------------
-- FamEqn type families
type family XCFamEqn      x r
type family XXFamEqn      x r

-- -------------------------------------
-- TyFamInstDecl type families
type family XCTyFamInstDecl x
type family XXTyFamInstDecl x

-- -------------------------------------
-- ClsInstDecl type families
type family XCClsInstDecl      x
type family XXClsInstDecl      x

-- -------------------------------------
-- InstDecl type families
type family XClsInstD      x
type family XDataFamInstD  x
type family XTyFamInstD    x
type family XXInstDecl     x

-- -------------------------------------
-- DerivDecl type families
type family XCDerivDecl      x
type family XXDerivDecl      x

-- -------------------------------------
-- DerivStrategy type family
type family XStockStrategy    x
type family XAnyClassStrategy x
type family XNewtypeStrategy  x
type family XViaStrategy      x

-- -------------------------------------
-- DefaultDecl type families
type family XCDefaultDecl      x
type family XXDefaultDecl      x

-- -------------------------------------
-- ForeignDecl type families
type family XForeignImport     x
type family XForeignExport     x
type family XXForeignDecl      x

-- -------------------------------------
-- RuleDecls type families
type family XCRuleDecls      x
type family XXRuleDecls      x

-- -------------------------------------
-- RuleDecl type families
type family XHsRule          x
type family XXRuleDecl       x

-- -------------------------------------
-- RuleBndr type families
type family XCRuleBndr      x
type family XRuleBndrSig    x
type family XXRuleBndr      x

-- -------------------------------------
-- WarnDecls type families
type family XWarnings        x
type family XXWarnDecls      x

-- -------------------------------------
-- WarnDecl type families
type family XWarning        x
type family XXWarnDecl      x

-- -------------------------------------
-- AnnDecl type families
type family XHsAnnotation  x
type family XXAnnDecl      x

-- -------------------------------------
-- RoleAnnotDecl type families
type family XCRoleAnnotDecl  x
type family XXRoleAnnotDecl  x

-- -------------------------------------
-- InjectivityAnn type families
type family XCInjectivityAnn  x
type family XXInjectivityAnn  x

-- =====================================================================
-- Type families for the HsExpr extension points

type family XVar            x
type family XUnboundVar     x
type family XRecSel         x
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
type family XGetField       x
type family XProjection     x
type family XExprWithTySig  x
type family XArithSeq       x
type family XBracket        x
type family XRnBracketOut   x
type family XTcBracketOut   x
type family XSpliceE        x
type family XProc           x
type family XStatic         x
type family XTick           x
type family XBinTick        x
type family XPragE          x
type family XXExpr          x

-- -------------------------------------
-- DotFieldOcc type families
type family XCDotFieldOcc  x
type family XXDotFieldOcc  x

-- -------------------------------------
-- HsPragE type families
type family XSCC            x
type family XXPragE         x


-- -------------------------------------
-- AmbiguousFieldOcc type families
type family XUnambiguous        x
type family XAmbiguous          x
type family XXAmbiguousFieldOcc x

-- -------------------------------------
-- HsTupArg type families
type family XPresent  x
type family XMissing  x
type family XXTupArg  x

-- -------------------------------------
-- HsSplice type families
type family XTypedSplice   x
type family XUntypedSplice x
type family XQuasiQuote    x
type family XSpliced       x
type family XXSplice       x

-- -------------------------------------
-- HsBracket type families
type family XExpBr      x
type family XPatBr      x
type family XDecBrL     x
type family XDecBrG     x
type family XTypBr      x
type family XVarBr      x
type family XTExpBr     x
type family XXBracket   x

-- -------------------------------------
-- HsCmdTop type families
type family XCmdTop  x
type family XXCmdTop x

-- -------------------------------------
-- MatchGroup type families
type family XMG           x b
type family XXMatchGroup  x b

-- -------------------------------------
-- Match type families
type family XCMatch  x b
type family XXMatch  x b

-- -------------------------------------
-- GRHSs type families
type family XCGRHSs  x b
type family XXGRHSs  x b

-- -------------------------------------
-- GRHS type families
type family XCGRHS  x b
type family XXGRHS  x b

-- -------------------------------------
-- StmtLR type families
type family XLastStmt        x x' b
type family XBindStmt        x x' b
type family XApplicativeStmt x x' b
type family XBodyStmt        x x' b
type family XLetStmt         x x' b
type family XParStmt         x x' b
type family XTransStmt       x x' b
type family XRecStmt         x x' b
type family XXStmtLR         x x' b

-- -------------------------------------
-- HsCmd type families
type family XCmdArrApp  x
type family XCmdArrForm x
type family XCmdApp     x
type family XCmdLam     x
type family XCmdPar     x
type family XCmdCase    x
type family XCmdLamCase x
type family XCmdIf      x
type family XCmdLet     x
type family XCmdDo      x
type family XCmdWrap    x
type family XXCmd       x

-- -------------------------------------
-- ParStmtBlock type families
type family XParStmtBlock  x x'
type family XXParStmtBlock x x'

-- -------------------------------------
-- ApplicativeArg type families
type family XApplicativeArgOne   x
type family XApplicativeArgMany  x
type family XXApplicativeArg     x

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

-- -------------------------------------
-- HsOverLit type families
type family XOverLit  x
type family XXOverLit x

-- =====================================================================
-- Type families for the HsPat extension points

type family XWildPat     x
type family XVarPat      x
type family XLazyPat     x
type family XAsPat       x
type family XParPat      x
type family XBangPat     x
type family XListPat     x
type family XTuplePat    x
type family XSumPat      x
type family XConPat      x
type family XViewPat     x
type family XSplicePat   x
type family XLitPat      x
type family XNPat        x
type family XNPlusKPat   x
type family XSigPat      x
type family XCoPat       x
type family XXPat        x
type family XHsFieldBind x
type family XVisPat      x
type family XInvisTyVarPat  x
type family XInvisWildTyPat x
type family XXMatchPat   x

-- =====================================================================
-- Type families for the HsTypes type families


-- -------------------------------------
-- LHsQTyVars type families
type family XHsQTvs       x
type family XXLHsQTyVars  x

-- -------------------------------------
-- HsOuterTyVarBndrs type families
type family XHsOuterImplicit    x
type family XHsOuterExplicit    x flag
type family XXHsOuterTyVarBndrs x

-- -------------------------------------
-- HsSigType type families
type family XHsSig      x
type family XXHsSigType x

-- -------------------------------------
-- HsWildCardBndrs type families
type family XHsWC              x b
type family XXHsWildCardBndrs  x b

-- -------------------------------------
-- HsPatSigType type families
type family XHsPS x
type family XXHsPatSigType x

-- -------------------------------------
-- HsType type families
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

-- ---------------------------------------------------------------------
-- HsForAllTelescope type families
type family XHsForAllVis        x
type family XHsForAllInvis      x
type family XXHsForAllTelescope x

-- ---------------------------------------------------------------------
-- HsTyVarBndr type families
type family XUserTyVar   x
type family XKindedTyVar x
type family XXTyVarBndr  x

-- ---------------------------------------------------------------------
-- ConDeclField type families
type family XConDeclField  x
type family XXConDeclField x

-- ---------------------------------------------------------------------
-- FieldOcc type families
type family XCFieldOcc x
type family XXFieldOcc x

-- =====================================================================
-- Type families for the HsImpExp type families

-- -------------------------------------
-- ImportDecl type families
type family XCImportDecl       x
type family XXImportDecl       x

-- -------------------------------------
-- IE type families
type family XIEVar             x
type family XIEThingAbs        x
type family XIEThingAll        x
type family XIEThingWith       x
type family XIEModuleContents  x
type family XIEGroup           x
type family XIEDoc             x
type family XIEDocNamed        x
type family XXIE               x

-- -------------------------------------

-- =====================================================================
-- Misc

-- | See Note [NoGhcTc] in GHC.Hs.Extension. It has to be in this
-- module because it is used like an extension point (in the data definitions
-- of types that should be parameter-agnostic.
type family NoGhcTc (p :: Type)

-- =====================================================================
-- End of Type family definitions
-- =====================================================================



-- =====================================================================
-- Token information

type LHsToken tok p = XRec p (HsToken tok)

data HsToken (tok :: Symbol) = HsTok

deriving instance KnownSymbol tok => Data (HsToken tok)

type LHsUniToken tok utok p = XRec p (HsUniToken tok utok)

-- With UnicodeSyntax, there might be multiple ways to write the same token.
-- For example an arrow could be either "->" or "→". This choice must be
-- recorded in order to exactprint such tokens,
-- so instead of HsToken "->" we introduce HsUniToken "->" "→".
--
-- See also IsUnicodeSyntax in GHC.Parser.Annotation; we do not use here to
-- avoid a dependency.
data HsUniToken (tok :: Symbol) (utok :: Symbol) = HsNormalTok | HsUnicodeTok

deriving instance (KnownSymbol tok, KnownSymbol utok) => Data (HsUniToken tok utok)
