{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module GHC.Hs.Extension
{-# LANGUAGE UndecidableSuperClasses #-}  -- for IsPass; see Note [NoGhcTc]
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}      -- for pprIfTc, etc.

module GHC.Hs.Extension where

-- This module captures the type families to precisely identify the extension
-- points for GHC.Hs syntax

import GhcPrelude

import Data.Data hiding ( Fixity )
import Name
import RdrName
import Var
import Outputable
import SrcLoc (Located)

import Data.Kind

{-
Note [Trees that grow]
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

See also Note [IsPass] and Note [NoGhcTc].

Note [IsPass]
~~~~~~~~~~~~~
One challenge with the Trees That Grow approach
is that we sometimes have different information in different passes.
For example, we have

  type instance XViaStrategy GhcPs = LHsSigType GhcPs
  type instance XViaStrategy GhcRn = LHsSigType GhcRn
  type instance XViaStrategy GhcTc = Type

This means that printing a DerivStrategy (which contains an XViaStrategy)
might need to print a LHsSigType, or it might need to print a type. Yet we
want one Outputable instance for a DerivStrategy, instead of one per pass. We
could have a large constraint, including e.g. (Outputable (XViaStrategy p),
Outputable (XViaStrategy GhcTc)), and pass that around in every context where
we might output a DerivStrategy. But a simpler alternative is to pass a
witness to whichever pass we're in. When we pattern-match on that (GADT)
witness, we learn the pass identity and can then print away. To wit, we get
the definition of GhcPass and the functions isPass. These allow us to do away
with big constraints, passing around all manner of dictionaries we might or
might not use. It does mean that we have to manually use isPass when printing,
but these places are few.

See Note [NoGhcTc] about the superclass constraint to IsPass.

Note [NoGhcTc]
~~~~~~~~~~~~~~
An expression is parsed into HsExpr GhcPs, renamed into HsExpr GhcRn, and
then type-checked into HsExpr GhcTc. Not so for types! These get parsed
into HsType GhcPs, renamed into HsType GhcRn, and then type-checked into
Type. We never build an HsType GhcTc. Why do this? Because we need to be
able to compare type-checked types for equality, and we don't want to do
this with HsType.

This causes wrinkles within the AST, where we normally thing that the whole
AST travels through the GhcPs --> GhcRn --> GhcTc pipeline as one. So we
have the NoGhcTc type family, which just replaces GhcTc with GhcRn, so that
user-written types can be preserved (as HsType GhcRn) even in e.g. HsExpr GhcTc.

For example, this is used in ExprWithTySig:
    | ExprWithTySig
                (XExprWithTySig p)

                (LHsExpr p)
                (LHsSigWcType (NoGhcTc p))

If we have (e :: ty), we still want to be able to print that (with the :: ty)
after type-checking. So we retain the LHsSigWcType GhcRn, even in an
HsExpr GhcTc. That's what NoGhcTc does.

When we're printing the type annotation, we need to know
(Outputable (LHsSigWcType GhcRn)), even though we've assumed only that
(OutputableBndrId GhcTc). We thus must be able to prove OutputableBndrId (NoGhcTc p)
from OutputableBndrId p. The extra constraints in OutputableBndrId and
the superclass constraints of IsPass allow this. Note that the superclass
constraint of IsPass is *recursive*: it asserts that IsPass (NoGhcTcPass p) holds.
For this to make sense, we need -XUndecidableSuperClasses and the other constraint,
saying that NoGhcTcPass is idempotent.

-}

-- | A placeholder type for TTG extension points that are not currently
-- unused to represent any particular value.
--
-- This should not be confused with 'NoExtCon', which are found in unused
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

-- | Used in TTG extension constructors that have yet to be extended with
-- anything. If an extension constructor has 'NoExtCon' as its field, it is
-- not intended to ever be constructed anywhere, and any function that consumes
-- the extension constructor can eliminate it by way of 'noExtCon'.
--
-- This should not be confused with 'NoExtField', which are found in unused
-- extension /points/ (not /constructors/) and therefore can be inhabited.

-- See also [NoExtCon and strict fields].
data NoExtCon
  deriving (Data,Eq,Ord)

instance Outputable NoExtCon where
  ppr = noExtCon

-- | Eliminate a 'NoExtCon'. Much like 'Data.Void.absurd'.
noExtCon :: NoExtCon -> a
noExtCon x = case x of {}

-- | GHC's L prefixed variants wrap their vanilla variant in this type family,
-- to add 'SrcLoc' info via 'Located'. Other passes than 'GhcPass' not
-- interested in location information can define this instance as @f p@.
type family XRec p (f :: * -> *) = r | r -> p f
type instance XRec (GhcPass p) f = Located (f (GhcPass p))

{-
Note [NoExtCon and strict fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Currently, any unused TTG extension constructor will generally look like the
following:

  type instance XXHsDecl (GhcPass _) = NoExtCon
  data HsDecl p
    = ...
    | XHsDecl (XXHsDecl p)

This means that any function that wishes to consume an HsDecl will need to
have a case for XHsDecl. This might look like this:

  ex :: HsDecl GhcPs -> HsDecl GhcRn
  ...
  ex (XHsDecl nec) = noExtCon nec

Ideally, we wouldn't need a case for XHsDecl at all (it /is/ supposed to be
an unused extension constructor, after all). There is a way to achieve this
on GHC 8.8 or later: make the field of XHsDecl strict:

  data HsDecl p
    = ...
    | XHsDecl !(XXHsDecl p)

If this is done, GHC's pattern-match coverage checker is clever enough to
figure out that the XHsDecl case of `ex` is unreachable, so it can simply be
omitted. (See Note [Extensions to GADTs Meet Their Match] in Check for more on
how this works.)

When GHC drops support for bootstrapping with GHC 8.6 and earlier, we can make
the strict field changes described above and delete gobs of code involving
`noExtCon`. Until then, it is necessary to use, so be aware of it when writing
code that consumes unused extension constructors.
-}

-- | Used as a data type index for the hsSyn AST; also serves
-- as a singleton type for Pass
data GhcPass (c :: Pass) where
  GhcPs :: GhcPs
  GhcRn :: GhcRn
  GhcTc :: GhcTc

-- This really should never be entered, but the data-deriving machinery
-- needs the instance to exist.
instance Typeable p => Data (GhcPass p) where
  gunfold _ _ _ = panic "instance Data GhcPass"
  toConstr  _   = panic "instance Data GhcPass"
  dataTypeOf _  = panic "instance Data GhcPass"

data Pass = Parsed | Renamed | Typechecked
         deriving (Data)

-- Type synonyms as a shorthand for tagging
type GhcPs   = GhcPass 'Parsed      -- Old 'RdrName' type param
type GhcRn   = GhcPass 'Renamed     -- Old 'Name' type param
type GhcTc   = GhcPass 'Typechecked -- Old 'Id' type para,
type GhcTcId = GhcTc                -- Old 'TcId' type param

-- | Allows us to check what phase we're in at GHC's runtime.
-- For example, this class allows us to write
-- >  f :: forall p. IsPass p => HsExpr (GhcPass p) -> blah
-- >  f e = case ghcPass @p of
-- >          GhcPs ->    ... in this RHS we have HsExpr GhcPs...
-- >          GhcRn ->    ... in this RHS we have HsExpr GhcRn...
-- >          GhcTc ->    ... in this RHS we have HsExpr GhcTc...
-- which is very useful, for example, when pretty-printing.
-- See Note [IsPass].
class ( NoGhcTcPass (NoGhcTcPass p) ~ NoGhcTcPass p
      , IsPass (NoGhcTcPass p)
      ) => IsPass p where
  ghcPass :: GhcPass p

instance IsPass 'Parsed where
  ghcPass = GhcPs
instance IsPass 'Renamed where
  ghcPass = GhcRn
instance IsPass 'Typechecked where
  ghcPass = GhcTc

-- | Maps the "normal" id type for a given pass
type family IdP p
type instance IdP (GhcPass p) = IdGhcP p

-- | Maps the "normal" id type for a given GHC pass
type family IdGhcP pass where
  IdGhcP 'Parsed      = RdrName
  IdGhcP 'Renamed     = Name
  IdGhcP 'Typechecked = Id

type LIdP p = Located (IdP p)

-- | Marks that a field uses the GhcRn variant even when the pass
-- parameter is GhcTc. Useful for storing HsTypes in GHC.Hs.Exprs, say, because
-- HsType GhcTc should never occur.
-- See Note [NoGhcTc]
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

-- ValBindsLR type families
type family XValBinds    x x'
type family XXValBindsLR x x'

-- HsBindsLR type families
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
-- ConDecl type families
type family XConDeclGADT   x
type family XConDeclH98    x
type family XXConDecl      x

-- -------------------------------------
-- FamEqn type families
type family XCFamEqn      x r
type family XXFamEqn      x r

-- -------------------------------------
-- ClsInstDecl type families
type family XCClsInstDecl      x
type family XXClsInstDecl      x

-- -------------------------------------
-- ClsInstDecl type families
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
type family XViaStrategy x

-- -------------------------------------
-- DefaultDecl type families
type family XCDefaultDecl      x
type family XXDefaultDecl      x

-- -------------------------------------
-- DefaultDecl type families
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
-- AnnDecl type families
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
type family XBracket        x
type family XRnBracketOut   x
type family XTcBracketOut   x
type family XSpliceE        x
type family XProc           x
type family XStatic         x
type family XTick           x
type family XBinTick        x
type family XPragE          x
type family XWrap           x
type family XXExpr          x

type family XSCC            x
type family XCoreAnn        x
type family XTickPragma     x
type family XXPragE         x
-- ---------------------------------------------------------------------

type family XUnambiguous        x
type family XAmbiguous          x
type family XXAmbiguousFieldOcc x

-- ----------------------------------------------------------------------

type family XPresent  x
type family XMissing  x
type family XXTupArg  x

-- ---------------------------------------------------------------------

type family XTypedSplice   x
type family XUntypedSplice x
type family XQuasiQuote    x
type family XSpliced       x
type family XXSplice       x

-- ---------------------------------------------------------------------

type family XExpBr      x
type family XPatBr      x
type family XDecBrL     x
type family XDecBrG     x
type family XTypBr      x
type family XVarBr      x
type family XTExpBr     x
type family XXBracket   x

-- ---------------------------------------------------------------------

type family XCmdTop  x
type family XXCmdTop x

-- -------------------------------------

type family XMG           x b
type family XXMatchGroup  x b

-- -------------------------------------

type family XCMatch  x b
type family XXMatch  x b

-- -------------------------------------

type family XCGRHSs  x b
type family XXGRHSs  x b

-- -------------------------------------

type family XCGRHS  x b
type family XXGRHS  x b

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

-- ---------------------------------------------------------------------

type family XParStmtBlock  x x'
type family XXParStmtBlock x x'

-- ---------------------------------------------------------------------

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

type family XOverLit  x
type family XXOverLit x

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
type family XConPatCon x
type family XViewPat   x
type family XSplicePat x
type family XLitPat    x
type family XNPat      x
type family XNPlusKPat x
type family XSigPat    x
type family XCoPat     x
type family XXPat      x

-- =====================================================================
-- Type families for the HsTypes type families

type family XHsQTvs       x
type family XXLHsQTyVars  x

-- -------------------------------------

type family XHsIB              x b
type family XXHsImplicitBndrs  x b

-- -------------------------------------

type family XHsWC              x b
type family XXHsWildCardBndrs  x b

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

-- ---------------------------------------------------------------------

type family XUserTyVar   x
type family XKindedTyVar x
type family XXTyVarBndr  x

-- ---------------------------------------------------------------------

type family XConDeclField  x
type family XXConDeclField x

-- ---------------------------------------------------------------------

type family XCFieldOcc x
type family XXFieldOcc x

-- =====================================================================
-- Type families for the HsImpExp type families

type family XCImportDecl       x
type family XXImportDecl       x

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

-- -------------------------------------


-- =====================================================================
-- End of Type family definitions
-- =====================================================================

-- |Constraint type to bundle up the requirement for 'OutputableBndr' on both
-- the @id@ and the 'NoGhcTc' of it. See Note [NoGhcTc].
type OutputableBndrId pass =
  ( OutputableBndr (IdGhcP pass)
  , OutputableBndr (IdGhcP (NoGhcTcPass pass))
  , IsPass pass
  )

-- useful helper functions:
pprIfPs :: forall p. IsPass p => (p ~ 'Parsed => SDoc) -> SDoc
pprIfPs pp = case ghcPass @p of GhcPs -> pp
                                _     -> empty

pprIfRn :: forall p. IsPass p => (p ~ 'Renamed => SDoc) -> SDoc
pprIfRn pp = case ghcPass @p of GhcRn -> pp
                                _     -> empty

pprIfTc :: forall p. IsPass p => (p ~ 'Typechecked => SDoc) -> SDoc
pprIfTc pp = case ghcPass @p of GhcTc -> pp
                                _     -> empty
