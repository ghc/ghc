{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension

{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

-- | Abstract Haskell syntax for expressions.
module GHC.Hs.Expr
  ( module Language.Haskell.Syntax.Expr
  , module GHC.Hs.Expr
  ) where

import Language.Haskell.Syntax.Expr

-- friends:
import GHC.Prelude

import GHC.Hs.Basic() -- import instances
import GHC.Hs.Decls() -- import instances
import GHC.Hs.Pat
import GHC.Hs.Lit
import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.Basic (FieldLabelString(..))
import GHC.Hs.Extension
import GHC.Hs.Type
import GHC.Hs.Binds
import GHC.Parser.Annotation

-- others:
import GHC.Tc.Types.Evidence
import GHC.Types.Id.Info ( RecSelParent )
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.Name.Set
import GHC.Types.Basic
import GHC.Types.Fixity
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Types.Tickish (CoreTickish)
import GHC.Types.Unique.Set (UniqSet)
import GHC.Types.ThLevelIndex
import GHC.Core.ConLike ( conLikeName, ConLike )
import GHC.Unit.Module (ModuleName)
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Data.FastString
import GHC.Core.Type
import GHC.Builtin.Types (mkTupleStr)
import GHC.Tc.Utils.TcType (TcType, TcTyVar)
import {-# SOURCE #-} GHC.Tc.Types.LclEnv (TcLclEnv)

import GHCi.RemoteTypes ( ForeignRef )
import qualified GHC.Boot.TH.Syntax as TH (Q)

-- libraries:
import Data.Data hiding (Fixity(..))
import qualified Data.Data as Data (Fixity(..))
import qualified Data.Kind
import Data.Maybe (isJust)
import Data.Foldable ( toList )
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Void (Void)
import qualified Data.Set as S
{- *********************************************************************
*                                                                      *
                Expressions proper
*                                                                      *
********************************************************************* -}

-- | Post-Type checking Expression
--
-- PostTcExpr is an evidence expression attached to the syntax tree by the
-- type checker (c.f. postTcType).
type PostTcExpr  = HsExpr GhcTc

-- | Post-Type checking Table
--
-- We use a PostTcTable where there are a bunch of pieces of evidence, more
-- than is convenient to keep individually.
type PostTcTable = [(Name, PostTcExpr)]

-------------------------

-- Defining SyntaxExpr in two stages allows for better type inference, because
-- we can declare SyntaxExprGhc to be injective (and closed). Without injectivity,
-- noSyntaxExpr would be ambiguous.
type instance SyntaxExpr (GhcPass p) = SyntaxExprGhc p

type family SyntaxExprGhc (p :: Pass) = (r :: Data.Kind.Type) | r -> p where
  SyntaxExprGhc 'Parsed      = NoExtField
  SyntaxExprGhc 'Renamed     = SyntaxExprRn
  SyntaxExprGhc 'Typechecked = SyntaxExprTc

-- | The function to use in rebindable syntax. See Note [NoSyntaxExpr].
data SyntaxExprRn = SyntaxExprRn (HsExpr GhcRn)
    -- Why is the payload not just a Name?
    -- See Note [Monad fail : Rebindable syntax, overloaded strings] in "GHC.Rename.Expr"
                  | NoSyntaxExprRn

-- | An expression with wrappers, used for rebindable syntax
--
-- This should desugar to
--
-- > syn_res_wrap $ syn_expr (syn_arg_wraps[0] arg0)
-- >                         (syn_arg_wraps[1] arg1) ...
--
-- where the actual arguments come from elsewhere in the AST.
data SyntaxExprTc = SyntaxExprTc { syn_expr      :: HsExpr GhcTc
                                 , syn_arg_wraps :: [HsWrapper]
                                 , syn_res_wrap  :: HsWrapper }
                  | NoSyntaxExprTc  -- See Note [NoSyntaxExpr]

-- | This is used for rebindable-syntax pieces that are too polymorphic
-- for tcSyntaxOp (trS_fmap and the mzip in ParStmt)
noExpr :: HsExpr (GhcPass p)
noExpr = HsLit noExtField (HsString (SourceText $ fsLit "noExpr") (fsLit "noExpr"))

noSyntaxExpr :: forall p. IsPass p => SyntaxExpr (GhcPass p)
                              -- Before renaming, and sometimes after
                              -- See Note [NoSyntaxExpr]
noSyntaxExpr = case ghcPass @p of
  GhcPs -> noExtField
  GhcRn -> NoSyntaxExprRn
  GhcTc -> NoSyntaxExprTc

-- | Make a 'SyntaxExpr GhcRn' from an expression
-- Used only in getMonadFailOp.
-- See Note [Monad fail : Rebindable syntax, overloaded strings] in "GHC.Rename.Expr"
mkSyntaxExpr :: HsExpr GhcRn -> SyntaxExprRn
mkSyntaxExpr = SyntaxExprRn

instance Outputable SyntaxExprRn where
  ppr (SyntaxExprRn expr) = ppr expr
  ppr NoSyntaxExprRn      = text "<no syntax expr>"

instance Outputable SyntaxExprTc where
  ppr (SyntaxExprTc { syn_expr      = expr
                    , syn_arg_wraps = arg_wraps
                    , syn_res_wrap  = res_wrap })
    = sdocOption sdocPrintExplicitCoercions $ \print_co ->
      getPprDebug $ \debug ->
      if debug || print_co
      then ppr expr <> braces (pprWithCommas ppr arg_wraps)
                    <> braces (ppr res_wrap)
      else ppr expr

  ppr NoSyntaxExprTc = text "<no syntax expr>"

-- | HsWrap appears only in typechecker output
data HsWrap hs_syn = HsWrap HsWrapper      -- the wrapper
                            (hs_syn GhcTc) -- the thing that is wrapped

deriving instance (Data (hs_syn GhcTc), Typeable hs_syn) => Data (HsWrap hs_syn)

-- ---------------------------------------------------------------------

data HsBracketTc = HsBracketTc
  { hsb_quote   :: HsQuote GhcRn        -- See Note [The life cycle of a TH quotation]
  , hsb_ty      :: Type
  , hsb_wrap    :: Maybe QuoteWrapper   -- The wrapper to apply type and dictionary argument to the quote.
  , hsb_splices :: [PendingTcSplice]    -- Output of the type checker is the *original*
                                        -- renamed expression, plus
                                        -- _typechecked_ splices to be
                                        -- pasted back in by the desugarer
  }

type instance XTypedBracket GhcPs = (BracketAnn (EpToken "[||") (EpToken "[e||"), EpToken "||]")
type instance XTypedBracket GhcRn = NoExtField
type instance XTypedBracket GhcTc = HsBracketTc
type instance XUntypedBracket GhcPs = NoExtField
type instance XUntypedBracket GhcRn = [PendingRnSplice] -- See Note [Pending Splices]
                                                        -- Output of the renamer is the *original* renamed expression,
                                                        -- plus _renamed_ splices to be type checked
type instance XUntypedBracket GhcTc = HsBracketTc

data BracketAnn noE hasE
  = BracketNoE noE
  | BracketHasE hasE
  deriving Data

instance (NoAnn n, NoAnn h) => NoAnn (BracketAnn n h) where
  noAnn = BracketNoE noAnn

-- ---------------------------------------------------------------------

-- API Annotations types

data EpAnnHsCase = EpAnnHsCase
      { hsCaseAnnCase :: EpToken "case"
      , hsCaseAnnOf   :: EpToken "of"
      } deriving Data

instance NoAnn EpAnnHsCase where
  noAnn = EpAnnHsCase noAnn noAnn

data EpAnnLam = EpAnnLam
      { epl_lambda :: EpToken "\\"      -- ^ Location of '\' keyword
      , epl_case   :: Maybe EpaLocation -- ^ Location of 'case' or
                                        -- 'cases' keyword, depending
                                        -- on related 'HsLamVariant'.
      } deriving Data

instance NoAnn EpAnnLam where
  noAnn = EpAnnLam noAnn noAnn

-- Record selectors at parse time are HsVar; they convert to HsRecSel
-- on renaming.
type instance XRecSel              GhcPs = DataConCantHappen
type instance XRecSel              GhcRn = NoExtField
type instance XRecSel              GhcTc = NoExtField

-- OverLabel not present in GhcTc pass; see GHC.Rename.Expr
-- Note [Handling overloaded and rebindable constructs]
type instance XOverLabel     GhcPs = SourceText
type instance XOverLabel     GhcRn = SourceText
type instance XOverLabel     GhcTc = DataConCantHappen

-- ---------------------------------------------------------------------

type instance XVar           (GhcPass _) = NoExtField

type instance XIPVar         GhcPs = NoExtField
type instance XIPVar         GhcRn = NoExtField
type instance XIPVar         GhcTc = DataConCantHappen
type instance XOverLitE      (GhcPass _) = NoExtField
type instance XLitE          (GhcPass _) = NoExtField
type instance XLam           (GhcPass _) = EpAnnLam
type instance XApp           (GhcPass _) = NoExtField

type instance XAppTypeE      GhcPs = EpToken "@"
type instance XAppTypeE      GhcRn = NoExtField
type instance XAppTypeE      GhcTc = Type

-- OpApp not present in GhcTc pass; see GHC.Rename.Expr
-- Note [Handling overloaded and rebindable constructs]
type instance XOpApp         GhcPs = NoExtField
type instance XOpApp         GhcRn = Fixity
type instance XOpApp         GhcTc = DataConCantHappen

-- SectionL, SectionR not present in GhcTc pass; see GHC.Rename.Expr
-- Note [Handling overloaded and rebindable constructs]
type instance XSectionL      GhcPs = NoExtField
type instance XSectionR      GhcPs = NoExtField
type instance XSectionL      GhcRn = NoExtField
type instance XSectionR      GhcRn = NoExtField
type instance XSectionL      GhcTc = DataConCantHappen
type instance XSectionR      GhcTc = DataConCantHappen


type instance XNegApp        GhcPs = EpToken "-"
type instance XNegApp        GhcRn = NoExtField
type instance XNegApp        GhcTc = NoExtField

type instance XPar           GhcPs = (EpToken "(", EpToken ")")
type instance XPar           GhcRn = NoExtField
type instance XPar           GhcTc = NoExtField

type instance XExplicitTuple GhcPs = (EpaLocation, EpaLocation)
type instance XExplicitTuple GhcRn = NoExtField
type instance XExplicitTuple GhcTc = NoExtField

type instance XExplicitSum   GhcPs = AnnExplicitSum
type instance XExplicitSum   GhcRn = NoExtField
type instance XExplicitSum   GhcTc = [Type]

type instance XCase          GhcPs = EpAnnHsCase
type instance XCase          GhcRn = HsMatchContextRn
type instance XCase          GhcTc = HsMatchContextRn

type instance XIf            GhcPs = AnnsIf
type instance XIf            GhcRn = NoExtField
type instance XIf            GhcTc = NoExtField

type instance XMultiIf       GhcPs = (EpToken "if", EpToken "{", EpToken "}")
type instance XMultiIf       GhcRn = NoExtField
type instance XMultiIf       GhcTc = Type

type instance XLet           GhcPs = (EpToken "let", EpToken "in")
type instance XLet           GhcRn = NoExtField
type instance XLet           GhcTc = NoExtField

type instance XDo            GhcPs = AnnList EpaLocation
type instance XDo            GhcRn = NoExtField
type instance XDo            GhcTc = Type

type instance XExplicitList  GhcPs = AnnList ()
type instance XExplicitList  GhcRn = NoExtField
type instance XExplicitList  GhcTc = Type
-- GhcPs: ExplicitList includes all source-level
--   list literals, including overloaded ones
-- GhcRn and GhcTc: ExplicitList used only for list literals
--   that denote Haskell's built-in lists.  Overloaded lists
--   have been expanded away in the renamer
-- See Note [Handling overloaded and rebindable constructs]
-- in  GHC.Rename.Expr

type instance XRecordCon     GhcPs = (Maybe (EpToken "{"), Maybe (EpToken "}"))
type instance XRecordCon     GhcRn = NoExtField
type instance XRecordCon     GhcTc = PostTcExpr   -- Instantiated constructor function

type instance XRecordUpd     GhcPs = (Maybe (EpToken "{"), Maybe (EpToken "}"))
type instance XRecordUpd     GhcRn = NoExtField
type instance XRecordUpd     GhcTc = DataConCantHappen
  -- We desugar record updates in the typechecker.
  -- See [Handling overloaded and rebindable constructs],
  -- and [Record Updates] in GHC.Tc.Gen.Expr.

-- | Information about the parent of a record update:
--
--  - the parent type constructor or pattern synonym,
--  - the relevant con-likes,
--  - the field labels.
data family HsRecUpdParent x

data instance HsRecUpdParent GhcPs
data instance HsRecUpdParent GhcRn
  = RnRecUpdParent
  { rnRecUpdLabels :: NonEmpty FieldGlobalRdrElt
  , rnRecUpdCons   :: UniqSet ConLikeName }
data instance HsRecUpdParent GhcTc
  = TcRecUpdParent
  { tcRecUpdParent :: RecSelParent
  , tcRecUpdLabels :: NonEmpty FieldGlobalRdrElt
  , tcRecUpdCons   :: UniqSet ConLike }

type instance XLHsRecUpdLabels GhcPs = NoExtField
type instance XLHsRecUpdLabels GhcRn = NonEmpty (HsRecUpdParent GhcRn)
                                      -- Possible parents for the record update.
type instance XLHsRecUpdLabels GhcTc = DataConCantHappen

type instance XLHsOLRecUpdLabels p = NoExtField

type instance XGetField     GhcPs = NoExtField
type instance XGetField     GhcRn = NoExtField
type instance XGetField     GhcTc = DataConCantHappen
-- HsGetField is eliminated by the renamer. See [Handling overloaded
-- and rebindable constructs].

type instance XProjection     GhcPs = AnnProjection
type instance XProjection     GhcRn = NoExtField
type instance XProjection     GhcTc = DataConCantHappen
-- HsProjection is eliminated by the renamer. See [Handling overloaded
-- and rebindable constructs].

type instance XExprWithTySig GhcPs = TokDcolon
type instance XExprWithTySig GhcRn = NoExtField
type instance XExprWithTySig GhcTc = NoExtField

type instance XArithSeq      GhcPs = AnnArithSeq
type instance XArithSeq      GhcRn = NoExtField
type instance XArithSeq      GhcTc = PostTcExpr

type instance XProc          (GhcPass _) = (EpToken "proc", TokRarrow)

type instance XStatic        GhcPs = EpToken "static"
type instance XStatic        GhcRn = NameSet
type instance XStatic        GhcTc = (NameSet, Type)
  -- Free variables and type of expression, this is stored for convenience as wiring in
  -- StaticPtr is a bit tricky (see #20150)

type instance XEmbTy         GhcPs = EpToken "type"
type instance XEmbTy         GhcRn = NoExtField
type instance XEmbTy         GhcTc = DataConCantHappen
  -- A free-standing HsEmbTy is an error.
  -- Valid usages are immediately desugared into Type.


{-
Note [Holes in expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
This Note explains how GHC uses the `HsHole` constructor.

`HsHole` is used to represent:

  - anonymous ("_") and named ("_x") holes in expressions,
  - unbound variables,
  - and parse errors.

A `HsHole` can be thought of as any thing which is not necessarily a valid or
fully defined program fragment, but for which a type can be derived.

Note that holes (wildcards) in types, and partial type signatures, are not
handled using the mechanisms described here. Instead, see
Note [The wildcard story for types] for the relevant information.


* User-facing behavior

  While GHC uses the same internal mechanism to derive the type for any
  `HsHole`, it gives different feedback to the user depending on the type of
  hole. For example, an anonymous hole of the form

    foo x = x && _

  gives the diagnostic

    Foo.hs:5:14: error: [GHC-88464]
      • Found hole: _ :: Bool
      • In the second argument of ‘(&&)’, namely ‘_’
        In the expression: x && _

  while an expression containing an unbound variable

    foo x = x && y

  gives

    Foo.hs:5:14: error: [GHC-88464]
      Variable not in scope: y :: Bool


* HsHole during parsing, renaming, and type checking

  The usage of `HsHole` during the three phases is listed below.

  - Anynomous holes, i.e. the user wrote "_":

      Parser        HsHole (HoleVar "_")
      Renamer       HsHole (HoleVar "_")
      Typechecker   HsHole (HoleVar "_", ref :: HoleExprRef)

  - Unbound variables and named holes; i.e. the user wrote "x" or "_x", where
    `x` or `_x` is not in scope. A variable with a leading underscore has no
    special meaning to the parser.

      Parser        HsVar "_x"
      Renamer       HsHole (HoleVar "_x")
      Typechecker   HsHole (HoleVar "_x", ref :: HoleExprRef)

  - Parse errors currently do not survive beyond the parser because an error is
    thrown after parsing. However, in the future GHC is intended to be tolerant
    of parse errors until the type checking phase to provide diagnostics similar
    to holes. This current singular case looks like this:

      Parser        HsHole HoleError

  Note that between anonymous holes, named holes, and unbound variables only the
  parsing phase is distinct, while during the renaming and type checking phases
  the cases are handled identically. The distinction that the user can observe
  is only introduced during final error reporting. There the `RdrName` is
  examined to see whether it starts with an underscore or not to determine
  whether the `HsHole` came from a hole or an out of scope variable.


* Contents of HoleExprRef

  The HoleExprRef type used in the type checking phase is a data structure
  containing:

   - The type of the hole.
   - A ref-cell that is filled in (by the typechecker) with an
     error thunk.   With -fdefer-type errors we use this as the
     value of the hole.
   - A Unique (see Note [Uniques and tags]).

* Typechecking holes

  When the typechecker encounters a `HsHole`, it returns one with the
  HoleExprRef, but also emits a `DelayedError` into the `WantedConstraints`.
  This DelayedError later triggers the error reporting, and the filling-in of
  the error thunk, in GHC.Tc.Errors.

  The user has the option of deferring errors until runtime with
  `-fdefer-type-errors`. In this case, the hole carries evidence in its
  `HoleExprRef`. This evidence is an erroring expression that prints an error
  and crashes at runtime.

* Desugaring holes

  During desugaring, the `(HsHole (HoleVar "x", ref))` is desugared by
  reading the ref-cell to find the error thunk evidence term, put there by the
  constraint solver.

* Wrinkles:

  - Prior to fixing #17812, we used to invent an Id to hold the erroring
    expression, and then bind it during type-checking. But this does not support
    representation-polymorphic out-of-scope identifiers. See
    typecheck/should_compile/T17812. We thus use the mutable-CoreExpr approach
    described above.

  - You might think that the type in the HoleExprRef is the same as the type of
    the hole. However, because the hole type (hole_ty) is rewritten with respect
    to givens, this might not be the case. That is, the hole_ty is always (~) to
    the type of the HoleExprRef, but they might not be `eqType`. We need the
    type of the generated evidence to match what is expected in the context of
    the hole, and so we must store these types separately.

  - We really don't need the whole HoleExprRef; just the IORef EvTerm would be
    enough. But then deriving a Data instance becomes impossible. Much, much
    easier just to define HoleExprRef with a Data instance and store the whole
    structure.
-}
-- | Expression Hole. See Note [Holes in expressions].
type instance XHole GhcPs = HoleKind
type instance XHole GhcRn = HoleKind
type instance XHole GhcTc = (HoleKind, HoleExprRef)

data HoleKind
  = HoleVar (LIdP GhcPs)
  | HoleError
  deriving Data

-- | The RdrName for an unnamed hole ("_").
unnamedHoleRdrName :: RdrName
unnamedHoleRdrName = mkUnqual varName (fsLit "_")


type instance XForAll        GhcPs = NoExtField
type instance XForAll        GhcRn = NoExtField
type instance XForAll        GhcTc = DataConCantHappen

type instance XQual          GhcPs = NoExtField
type instance XQual          GhcRn = NoExtField
type instance XQual          GhcTc = DataConCantHappen

type instance XFunArr        GhcPs = NoExtField
type instance XFunArr        GhcRn = NoExtField
type instance XFunArr        GhcTc = DataConCantHappen

type instance XPragE         (GhcPass _) = NoExtField

type instance XFunRhs  = AnnFunRhs

type instance Anno [LocatedA ((StmtLR (GhcPass pl) (GhcPass pr) (LocatedA (body (GhcPass pr)))))] = SrcSpanAnnLW
type instance Anno (StmtLR GhcRn GhcRn (LocatedA (body GhcRn))) = SrcSpanAnnA

multAnnToHsExpr :: HsMultAnnOf (LocatedA (HsExpr GhcRn)) GhcRn -> Maybe (LocatedA (HsExpr GhcRn))
multAnnToHsExpr = expandHsMultAnnOf mkHsVar

mkHsVar :: forall p. IsPass p => LIdP (GhcPass p) -> HsExpr (GhcPass p)
mkHsVar n = HsVar noExtField $
  case ghcPass @p of
    GhcPs -> n
    GhcRn -> fmap (WithUserRdr $ nameRdrName $ unLoc n) n
    GhcTc -> n

mkHsVarWithUserRdr :: forall p. IsPass p => RdrName -> LIdP (GhcPass p) -> HsExpr (GhcPass p)
mkHsVarWithUserRdr rdr n = HsVar noExtField $
  case ghcPass @p of
    GhcPs -> n
    GhcRn -> fmap (WithUserRdr rdr) n
    GhcTc -> n

data AnnExplicitSum
  = AnnExplicitSum {
      aesOpen       :: EpaLocation,
      aesBarsBefore :: [EpToken "|"],
      aesBarsAfter  :: [EpToken "|"],
      aesClose      :: EpaLocation
      } deriving Data

instance NoAnn AnnExplicitSum where
  noAnn = AnnExplicitSum noAnn noAnn noAnn noAnn

data AnnFieldLabel
  = AnnFieldLabel {
      afDot :: Maybe (EpToken ".")
      } deriving Data

instance NoAnn AnnFieldLabel where
  noAnn = AnnFieldLabel Nothing

data AnnProjection
  = AnnProjection {
      apOpen  :: EpToken "(",
      apClose :: EpToken ")"
      } deriving Data

instance NoAnn AnnProjection where
  noAnn = AnnProjection noAnn noAnn

data AnnArithSeq
  = AnnArithSeq {
      aas_open   :: EpToken "[",
      aas_comma  :: Maybe (EpToken ","),
      aas_dotdot :: EpToken "..",
      aas_close  :: EpToken "]"
      } deriving Data

instance NoAnn AnnArithSeq where
  noAnn = AnnArithSeq noAnn noAnn noAnn noAnn

data AnnsIf
  = AnnsIf {
      aiIf       :: EpToken "if",
      aiThen     :: EpToken "then",
      aiElse     :: EpToken "else",
      aiThenSemi :: Maybe (EpToken ";"),
      aiElseSemi :: Maybe (EpToken ";")
      } deriving Data

instance NoAnn AnnsIf where
  noAnn = AnnsIf noAnn noAnn noAnn Nothing Nothing

data AnnFunRhs
  = AnnFunRhs {
       afr_strict :: EpToken "!",
       afr_opens  :: [EpToken "("],
       afr_closes :: [EpToken ")"]
  } deriving Data

instance NoAnn AnnFunRhs where
  noAnn = AnnFunRhs noAnn noAnn noAnn

-- ---------------------------------------------------------------------

type instance XSCC           (GhcPass _) = (AnnPragma, SourceText)
type instance XXPragE        (GhcPass _) = DataConCantHappen

type instance XCDotFieldOcc (GhcPass _) = AnnFieldLabel
type instance XXDotFieldOcc (GhcPass _) = DataConCantHappen

type instance XPresent         (GhcPass _) = NoExtField

type instance XMissing         GhcPs = EpAnn Bool -- True for empty last comma
type instance XMissing         GhcRn = NoExtField
type instance XMissing         GhcTc = Scaled Type

type instance XXTupArg         (GhcPass _) = DataConCantHappen

tupArgPresent :: HsTupArg (GhcPass p) -> Bool
tupArgPresent (Present {}) = True
tupArgPresent (Missing {}) = False

tupArgPresent_maybe :: HsTupArg (GhcPass p) -> Maybe (LHsExpr (GhcPass p))
tupArgPresent_maybe (Present _ e) = Just e
tupArgPresent_maybe (Missing {})  = Nothing

tupArgsPresent_maybe :: [HsTupArg (GhcPass p)] -> Maybe [LHsExpr (GhcPass p)]
tupArgsPresent_maybe = traverse tupArgPresent_maybe


{- *********************************************************************
*                                                                      *
            XXExpr: the extension constructor of HsExpr
*                                                                      *
********************************************************************* -}

type instance XXExpr GhcPs = DataConCantHappen
type instance XXExpr GhcRn = XXExprGhcRn
type instance XXExpr GhcTc = XXExprGhcTc
-- XXExprGhcRn: see Note [Rebindable syntax and XXExprGhcRn] below


{- *********************************************************************
*                                                                      *
              Generating code for ExpandedThingRn
      See Note [Handling overloaded and rebindable constructs]
*                                                                      *
********************************************************************* -}

-- | The different source constructs that we use to instantiate the "original" field
--   in an `XXExprGhcRn original expansion`
data HsThingRn = OrigExpr (HsExpr GhcRn)
               | OrigStmt (ExprLStmt GhcRn)
               | OrigPat  (LPat GhcRn)

isHsThingRnExpr, isHsThingRnStmt, isHsThingRnPat :: HsThingRn -> Bool
isHsThingRnExpr (OrigExpr{}) = True
isHsThingRnExpr _ = False

isHsThingRnStmt (OrigStmt{}) = True
isHsThingRnStmt _ = False

isHsThingRnPat (OrigPat{}) = True
isHsThingRnPat _ = False

data XXExprGhcRn
  = ExpandedThingRn { xrn_orig     :: HsThingRn       -- The original source thing
                    , xrn_expanded :: HsExpr GhcRn }  -- The compiler generated expanded thing

  | PopErrCtxt                                     -- A hint for typechecker to pop
    {-# UNPACK #-} !(LHsExpr GhcRn)                -- the top of the error context stack
                                                   -- Does not presist post renaming phase
                                                   -- See Part 3. of Note [Expanding HsDo with XXExprGhcRn]
                                                   -- in `GHC.Tc.Gen.Do`
  | HsRecSelRn  (FieldOcc GhcRn)   -- ^ Variable pointing to record selector
                           -- See Note [Non-overloaded record field selectors] and
                           -- Note [Record selectors in the AST]



-- | Wrap a located expression with a `PopErrCtxt`
mkPopErrCtxtExpr :: LHsExpr GhcRn -> HsExpr GhcRn
mkPopErrCtxtExpr a = XExpr (PopErrCtxt a)

-- | Wrap a located expression with a PopSrcExpr with an appropriate location
mkPopErrCtxtExprAt :: SrcSpanAnnA ->  LHsExpr GhcRn -> LHsExpr GhcRn
mkPopErrCtxtExprAt loc a = L loc $ mkPopErrCtxtExpr a

-- | Build an expression using the extension constructor `XExpr`,
--   and the two components of the expansion: original expression and
--   expanded expressions.
mkExpandedExpr
  :: HsExpr GhcRn         -- ^ source expression
  -> HsExpr GhcRn         -- ^ expanded expression
  -> HsExpr GhcRn         -- ^ suitably wrapped 'XXExprGhcRn'
mkExpandedExpr oExpr eExpr = XExpr (ExpandedThingRn (OrigExpr oExpr) eExpr)

-- | Build an expression using the extension constructor `XExpr`,
--   and the two components of the expansion: original do stmt and
--   expanded expression
mkExpandedStmt
  :: ExprLStmt GhcRn      -- ^ source statement
  -> HsExpr GhcRn         -- ^ expanded expression
  -> HsExpr GhcRn         -- ^ suitably wrapped 'XXExprGhcRn'
mkExpandedStmt oStmt eExpr = XExpr (ExpandedThingRn (OrigStmt oStmt) eExpr)

mkExpandedPatRn
  :: LPat   GhcRn      -- ^ source pattern
  -> HsExpr GhcRn      -- ^ expanded expression
  -> HsExpr GhcRn      -- ^ suitably wrapped 'XXExprGhcRn'
mkExpandedPatRn oPat eExpr = XExpr (ExpandedThingRn (OrigPat oPat) eExpr)

-- | Build an expression using the extension constructor `XExpr`,
--   and the two components of the expansion: original do stmt and
--   expanded expression an associate with a provided location
mkExpandedStmtAt
  :: SrcSpanAnnA          -- ^ Location for the expansion expression
  -> ExprLStmt GhcRn      -- ^ source statement
  -> HsExpr GhcRn         -- ^ expanded expression
  -> LHsExpr GhcRn        -- ^ suitably wrapped located 'XXExprGhcRn'
mkExpandedStmtAt loc oStmt eExpr = L loc $ mkExpandedStmt oStmt eExpr

-- | Wrap the expanded version of the expression with a pop.
mkExpandedStmtPopAt
  :: SrcSpanAnnA          -- ^ Location for the expansion statement
  -> ExprLStmt GhcRn      -- ^ source statement
  -> HsExpr GhcRn         -- ^ expanded expression
  -> LHsExpr GhcRn        -- ^ suitably wrapped 'XXExprGhcRn'
mkExpandedStmtPopAt loc oStmt eExpr = mkPopErrCtxtExprAt loc $ mkExpandedStmtAt loc oStmt eExpr


data XXExprGhcTc
  = WrapExpr        -- Type and evidence application and abstractions
      HsWrapper (HsExpr GhcTc)

  | ExpandedThingTc                         -- See Note [Rebindable syntax and XXExprGhcRn]
                                            -- See Note [Expanding HsDo with XXExprGhcRn] in `GHC.Tc.Gen.Do`
         { xtc_orig     :: HsThingRn        -- The original user written thing
         , xtc_expanded :: HsExpr GhcTc }   -- The expanded typechecked expression

  | ConLikeTc      -- Result of typechecking a data-con
                   -- See Note [Typechecking data constructors] in
                   --     GHC.Tc.Gen.Head
                   -- The two arguments describe how to eta-expand
                   -- the data constructor when desugaring
        ConLike [TcTyVar] [Scaled TcType]

  ---------------------------------------
  -- Haskell program coverage (Hpc) Support

  | HsTick
     CoreTickish
     (LHsExpr GhcTc)                    -- sub-expression

  | HsBinTick
     Int                                -- module-local tick number for True
     Int                                -- module-local tick number for False
     (LHsExpr GhcTc)                    -- sub-expression

  | HsRecSelTc  (FieldOcc GhcTc) -- ^ Variable pointing to record selector
                             -- See Note [Non-overloaded record field selectors] and
                             -- Note [Record selectors in the AST]


-- | Build a 'XXExprGhcRn' out of an extension constructor,
--   and the two components of the expansion: original and
--   expanded typechecked expressions.
mkExpandedExprTc
  :: HsExpr GhcRn           -- ^ source expression
  -> HsExpr GhcTc           -- ^ expanded typechecked expression
  -> HsExpr GhcTc           -- ^ suitably wrapped 'XXExprGhcRn'
mkExpandedExprTc oExpr eExpr = XExpr (ExpandedThingTc (OrigExpr oExpr) eExpr)

-- | Build a 'XXExprGhcRn' out of an extension constructor.
--   The two components of the expansion are: original statement and
--   expanded typechecked expression.
mkExpandedStmtTc
  :: ExprLStmt GhcRn        -- ^ source do statement
  -> HsExpr GhcTc           -- ^ expanded typechecked expression
  -> HsExpr GhcTc           -- ^ suitably wrapped 'XXExprGhcRn'
mkExpandedStmtTc oStmt eExpr = XExpr (ExpandedThingTc (OrigStmt oStmt) eExpr)

{- *********************************************************************
*                                                                      *
            Pretty-printing expressions
*                                                                      *
********************************************************************* -}

instance (OutputableBndrId p) => Outputable (HsExpr (GhcPass p)) where
    ppr expr = pprExpr expr

-----------------------
-- pprExpr, pprLExpr, pprBinds call pprDeeper;
-- the underscore versions do not
pprLExpr :: (OutputableBndrId p) => LHsExpr (GhcPass p) -> SDoc
pprLExpr (L _ e) = pprExpr e

pprExpr :: (OutputableBndrId p) => HsExpr (GhcPass p) -> SDoc
pprExpr e | isAtomicHsExpr e || isQuietHsExpr e =            ppr_expr e
          | otherwise                           = pprDeeper (ppr_expr e)

isQuietHsExpr :: HsExpr id -> Bool
-- Parentheses do display something, but it gives little info and
-- if we go deeper when we go inside them then we get ugly things
-- like (...)
isQuietHsExpr (HsPar {})        = True
-- applications don't display anything themselves
isQuietHsExpr (HsApp {})        = True
isQuietHsExpr (HsAppType {})    = True
isQuietHsExpr (OpApp {})        = True
isQuietHsExpr _ = False

pprBinds :: (OutputableBndrId idL, OutputableBndrId idR)
         => HsLocalBindsLR (GhcPass idL) (GhcPass idR) -> SDoc
pprBinds b = pprDeeper (ppr b)

-----------------------
ppr_lexpr :: (OutputableBndrId p) => LHsExpr (GhcPass p) -> SDoc
ppr_lexpr e = ppr_expr (unLoc e)

ppr_expr :: forall p. (OutputableBndrId p)
         => HsExpr (GhcPass p) -> SDoc
ppr_expr (HsVar _ (L _ v))   = pprPrefixOcc v
ppr_expr (HsHole x) = case (ghcPass @p, x) of
  (GhcPs, HoleVar (L _ v)) -> pprPrefixOcc v
  (GhcRn, HoleVar (L _ v)) -> pprPrefixOcc v
  (GhcTc, (HoleVar (L _ v), _)) -> pprPrefixOcc v
  (GhcPs, HoleError) -> pprPrefixOcc unnamedHoleRdrName
  (GhcRn, HoleError) -> pprPrefixOcc unnamedHoleRdrName
  (GhcTc, (HoleError, _)) -> pprPrefixOcc unnamedHoleRdrName
ppr_expr (HsIPVar _ v)       = ppr v
ppr_expr (HsOverLabel s l) = case ghcPass @p of
               GhcPs -> helper s
               GhcRn -> helper s
               GhcTc -> dataConCantHappen s
    where helper s =
            char '#' <> case s of
                          NoSourceText -> ppr l
                          SourceText src -> ftext src
ppr_expr (HsLit _ lit)       = ppr lit
ppr_expr (HsOverLit _ lit)   = ppr lit
ppr_expr (HsPar _ e)         = parens (ppr_lexpr e)

ppr_expr (HsPragE _ prag e) = sep [ppr prag, ppr_lexpr e]

ppr_expr e@(HsApp {})        = ppr_apps e []
ppr_expr e@(HsAppType {})    = ppr_apps e []

ppr_expr (OpApp _ e1 op e2)
  | Just pp_op <- ppr_infix_expr (unLoc op)
  = pp_infixly pp_op
  | otherwise
  = pp_prefixly

  where
    pp_e1 = pprDebugParendExpr opPrec e1   -- In debug mode, add parens
    pp_e2 = pprDebugParendExpr opPrec e2   -- to make precedence clear

    pp_prefixly
      = hang (ppr op) 2 (sep [pp_e1, pp_e2])

    pp_infixly pp_op
      = hang pp_e1 2 (sep [pp_op, nest 2 pp_e2])

ppr_expr (NegApp _ e _) = char '-' <+> pprDebugParendExpr appPrec e

ppr_expr (SectionL _ expr op)
  | Just pp_op <- ppr_infix_expr (unLoc op)
  = pp_infixly pp_op
  | otherwise
  = pp_prefixly
  where
    pp_expr = pprDebugParendExpr opPrec expr

    pp_prefixly = hang (hsep [text " \\ x_ ->", ppr op])
                       4 (hsep [pp_expr, text "x_ )"])

    pp_infixly v = (sep [pp_expr, v])

ppr_expr (SectionR _ op expr)
  | Just pp_op <- ppr_infix_expr (unLoc op)
  = pp_infixly pp_op
  | otherwise
  = pp_prefixly
  where
    pp_expr = pprDebugParendExpr opPrec expr

    pp_prefixly = hang (hsep [text "( \\ x_ ->", ppr op, text "x_"])
                       4 (pp_expr <> rparen)

    pp_infixly v = sep [v, pp_expr]

ppr_expr (ExplicitTuple _ exprs boxity)
    -- Special-case unary boxed tuples so that they are pretty-printed as
    -- `MkSolo x`, not `(x)`
  | [Present _ expr] <- exprs
  , Boxed <- boxity
  = hsep [text (mkTupleStr Boxed dataName 1), ppr expr]
  | otherwise
  = tupleParens (boxityTupleSort boxity) (fcat (ppr_tup_args exprs))
  where
    ppr_tup_args []               = []
    ppr_tup_args (Present _ e : es) = (ppr_lexpr e <> punc es) : ppr_tup_args es
    ppr_tup_args (Missing _   : es) = punc es : ppr_tup_args es

    punc (Present {} : _) = comma <> space
    punc (Missing {} : _) = comma
    punc (XTupArg {} : _) = comma <> space
    punc []               = empty

ppr_expr (ExplicitSum _ alt arity expr)
  = text "(#" <+> ppr_bars (alt - 1) <+> ppr expr <+> ppr_bars (arity - alt) <+> text "#)"
  where
    ppr_bars n = hsep (replicate n (char '|'))

ppr_expr (HsLam _ lam_variant matches)
  = case lam_variant of
       LamSingle -> pprMatches matches
       _         -> sep [ sep [lamCaseKeyword lam_variant]
                        , nest 2 (pprMatches matches) ]

ppr_expr (HsCase _ expr matches@(MG { mg_alts = L _ alts }))
  = sep [ sep [text "case", nest 4 (ppr expr), text "of"],
          pp_alts ]
  where
    pp_alts | null alts = text "{}"
            | otherwise = nest 2 (pprMatches matches)

ppr_expr (HsIf _ e1 e2 e3)
  = sep [hsep [text "if", nest 2 (ppr e1), text "then"],
         nest 4 (ppr e2),
         text "else",
         nest 4 (ppr e3)]

ppr_expr (HsMultiIf _ alts)
  = hang (text "if") 3  (vcat $ toList $ NE.map ppr_alt alts)
  where ppr_alt (L _ (GRHS _ guards expr)) =
          hang vbar 2 (hang (interpp'SP guards) 2 (arrow <+> pprDeeper (ppr expr)))
        ppr_alt (L _ (XGRHS x)) = ppr x

-- special case: let ... in let ...
ppr_expr (HsLet _ binds expr@(L _ (HsLet _ _ _)))
  = sep [hang (text "let") 2 (hsep [pprBinds binds, text "in"]),
         ppr_lexpr expr]

ppr_expr (HsLet _ binds expr)
  = sep [hang (text "let") 2 (pprBinds binds),
         hang (text "in")  2 (ppr expr)]

ppr_expr (HsDo _ do_or_list_comp (L _ stmts)) = pprDo do_or_list_comp stmts

ppr_expr (ExplicitList _ exprs)
  = brackets (pprDeeperList fsep (punctuate comma (map ppr_lexpr exprs)))

ppr_expr (RecordCon { rcon_con = con, rcon_flds = rbinds })
  = hang pp_con 2 (ppr rbinds)
  where
    -- con :: ConLikeP (GhcPass p)
    -- so we need case analysis to know to print it
    pp_con = case ghcPass @p of
               GhcPs -> ppr con
               GhcRn -> ppr con
               GhcTc -> ppr con

ppr_expr (RecordUpd { rupd_expr = L _ aexp, rupd_flds = flds })
  = case flds of
      RegularRecUpdFields { recUpdFields= rbinds } ->
        hang (ppr aexp) 2 (braces (fsep (punctuate comma (map ppr rbinds))))
      OverloadedRecUpdFields { olRecUpdFields = pbinds } ->
        hang (ppr aexp) 2 (braces (fsep (punctuate comma (map ppr pbinds))))

ppr_expr (HsGetField { gf_expr = L _ fexp, gf_field = field })
  = ppr fexp <> dot <> ppr field

ppr_expr (HsProjection { proj_flds = flds }) = parens (hcat (dot : (punctuate dot (map ppr $ toList flds))))

ppr_expr (ExprWithTySig _ expr sig)
  = hang (nest 2 (ppr_lexpr expr) <+> dcolon)
         4 (ppr sig)

ppr_expr (ArithSeq _ _ info) = brackets (ppr info)

ppr_expr (HsTypedSplice ext e)   =
    case ghcPass @p of
      GhcPs -> pprTypedSplice Nothing e
      GhcRn ->
        case ext of
          HsTypedSpliceNested n -> pprTypedSplice (Just n) e
          HsTypedSpliceTop {}   -> pprTypedSplice Nothing e
      GhcTc -> pprTypedSplice Nothing e
ppr_expr (HsUntypedSplice ext s) =
    case ghcPass @p of
      GhcPs -> pprUntypedSplice True Nothing s
      GhcRn | HsUntypedSpliceNested n <- ext -> pprUntypedSplice True (Just n) s
      GhcRn | HsUntypedSpliceTop _ e  <- ext -> ppr e
      GhcTc -> dataConCantHappen ext

ppr_expr (HsTypedBracket b e)
  = case ghcPass @p of
    GhcPs -> thTyBrackets (ppr e)
    GhcRn -> thTyBrackets (ppr e)
    GhcTc | HsBracketTc _  _ty _wrap ps <- b ->
      thTyBrackets (ppr e) `ppr_with_pending_tc_splices` ps
ppr_expr (HsUntypedBracket b q)
  = case ghcPass @p of
    GhcPs -> ppr q
    GhcRn -> case b of
      [] -> ppr q
      ps -> ppr q $$ whenPprDebug (text "pending(rn)" <+> ppr (map ppr_nested_splice ps))
    GhcTc | HsBracketTc rnq  _ty _wrap ps <- b ->
      ppr rnq `ppr_with_pending_tc_splices` ps
  where
    ppr_nested_splice (PendingRnSplice splice_name expr) = pprUntypedSplice False (Just splice_name) expr

ppr_expr (HsProc _ pat (L _ (HsCmdTop _ cmd)))
  = hsep [text "proc", ppr pat, arrow, ppr cmd]

ppr_expr (HsStatic _ e)
  = hsep [text "static", ppr e]

ppr_expr (HsEmbTy _ ty)
  = hsep [text "type", ppr ty]

ppr_expr (HsQual _ ctxt ty)
  = sep [ppr_context ctxt, ppr_lexpr ty]
  where
    ppr_context (L _ ctxt) =
      case ctxt of
        []       -> parens empty             <+> darrow
        [L _ ty] -> ppr_expr ty              <+> darrow
        _        -> parens (interpp'SP ctxt) <+> darrow

ppr_expr (HsForAll _ tele ty)
  = sep [pprHsForAll tele Nothing, ppr_lexpr ty]

ppr_expr (HsFunArr _ arr arg res)
  = sep [ppr_lexpr arg, pprHsArrow arr <+> ppr_lexpr res]

ppr_expr (XExpr x) = case ghcPass @p of
  GhcRn -> ppr x
  GhcTc -> ppr x

instance Outputable HsThingRn where
  ppr thing
    = case thing of
        OrigExpr x -> ppr_builder "<OrigExpr>:" x
        OrigStmt x -> ppr_builder "<OrigStmt>:" x
        OrigPat x  -> ppr_builder "<OrigPat>:" x

    where ppr_builder prefix x = ifPprDebug (braces (text prefix <+> parens (ppr x))) (ppr x)

instance Outputable XXExprGhcRn where
  ppr (ExpandedThingRn o e) = ifPprDebug (braces $ vcat [ppr o, ppr e]) (ppr o)
  ppr (PopErrCtxt e)        = ifPprDebug (braces (text "<PopErrCtxt>" <+> ppr e)) (ppr e)
  ppr (HsRecSelRn f)        = pprPrefixOcc f

instance Outputable XXExprGhcTc where
  ppr (WrapExpr co_fn e)
    = pprHsWrapper co_fn (\_parens -> pprExpr e)

  ppr (ExpandedThingTc o e)
    = ifPprDebug (braces $ vcat [ppr o, ppr e]) (ppr o)
            -- e is the expanded expression, we print the original
            -- expression (HsExpr GhcRn), not the
            -- expanded typechecked one (HsExpr GhcTc),
            -- unless we are in ppr's debug mode printed both

  ppr (ConLikeTc con _ _) = pprPrefixOcc con
   -- Used in error messages generated by
   -- the pattern match overlap checker

  ppr (HsTick tickish exp) =
    pprTicks (ppr exp) $
      ppr tickish <+> ppr_lexpr exp

  ppr (HsBinTick tickIdTrue tickIdFalse exp) =
    pprTicks (ppr exp) $
      hcat [text "bintick<",
            ppr tickIdTrue,
            text ",",
            ppr tickIdFalse,
            text ">(",
            ppr exp, text ")"]
  ppr (HsRecSelTc f)      = pprPrefixOcc f

ppr_infix_expr :: forall p. (OutputableBndrId p) => HsExpr (GhcPass p) -> Maybe SDoc
ppr_infix_expr (HsVar _ (L _ v))    = Just (pprInfixOcc v)
ppr_infix_expr (HsHole x) = Just $ pprInfixOcc $ case (ghcPass @p, x) of
  (GhcPs, HoleVar (L _ v)) -> v
  (GhcRn, HoleVar (L _ v)) -> v
  (GhcTc, (HoleVar (L _ v), _)) -> v
  _ -> unnamedHoleRdrName -- TODO: this is the HoleError case; this should print the source text instead of "_".
ppr_infix_expr (XExpr x)            = case ghcPass @p of
                                        GhcRn -> ppr_infix_expr_rn x
                                        GhcTc -> ppr_infix_expr_tc x
ppr_infix_expr _ = Nothing

ppr_infix_expr_rn :: XXExprGhcRn -> Maybe SDoc
ppr_infix_expr_rn (ExpandedThingRn thing _) = ppr_infix_hs_expansion thing
ppr_infix_expr_rn (PopErrCtxt (L _ a))      = ppr_infix_expr a
ppr_infix_expr_rn (HsRecSelRn f)            = Just (pprInfixOcc f)

ppr_infix_expr_tc :: XXExprGhcTc -> Maybe SDoc
ppr_infix_expr_tc (WrapExpr _ e)    = ppr_infix_expr e
ppr_infix_expr_tc (ExpandedThingTc thing _)  = ppr_infix_hs_expansion thing
ppr_infix_expr_tc (ConLikeTc {})             = Nothing
ppr_infix_expr_tc (HsTick {})                = Nothing
ppr_infix_expr_tc (HsBinTick {})             = Nothing
ppr_infix_expr_tc (HsRecSelTc f)            = Just (pprInfixOcc f)

ppr_infix_hs_expansion :: HsThingRn -> Maybe SDoc
ppr_infix_hs_expansion (OrigExpr e) = ppr_infix_expr e
ppr_infix_hs_expansion _            = Nothing

ppr_apps :: (OutputableBndrId p)
         => HsExpr (GhcPass p)
         -> [Either (LHsExpr (GhcPass p)) (LHsWcType (NoGhcTc (GhcPass p)))]
         -> SDoc
ppr_apps (HsApp _ (L _ fun) arg)        args
  = ppr_apps fun (Left arg : args)
ppr_apps (HsAppType _ (L _ fun) arg)    args
  = ppr_apps fun (Right arg : args)
ppr_apps fun args = hang (ppr_expr fun) 2 (fsep (map pp args))
  where
    pp (Left arg)                             = ppr arg
    -- pp (Right (LHsWcTypeX (HsWC { hswc_body = L _ arg })))
    --   = char '@' <> pprHsType arg
    pp (Right arg)
      = text "@" <> ppr arg

pprDebugParendExpr :: (OutputableBndrId p)
                   => PprPrec -> LHsExpr (GhcPass p) -> SDoc
pprDebugParendExpr p expr
  = getPprDebug $ \case
      True  -> pprParendLExpr p expr
      False -> pprLExpr         expr

pprParendLExpr :: (OutputableBndrId p)
               => PprPrec -> LHsExpr (GhcPass p) -> SDoc
pprParendLExpr p (L _ e) = pprParendExpr p e

pprParendExpr :: (OutputableBndrId p)
              => PprPrec -> HsExpr (GhcPass p) -> SDoc
pprParendExpr p expr
  | hsExprNeedsParens p expr = parens (pprExpr expr)
  | otherwise                = pprExpr expr
        -- Using pprLExpr makes sure that we go 'deeper'
        -- I think that is usually (always?) right

-- | @'hsExprNeedsParens' p e@ returns 'True' if the expression @e@ needs
-- parentheses under precedence @p@.
hsExprNeedsParens :: forall p. IsPass p => PprPrec -> HsExpr (GhcPass p) -> Bool
hsExprNeedsParens prec = go
  where
    go :: HsExpr (GhcPass p) -> Bool
    go (HsVar{})                      = False
    go (HsIPVar{})                    = False
    go (HsOverLabel{})                = False
    go (HsLit _ l)                    = hsLitNeedsParens prec l
    go (HsOverLit _ ol)               = hsOverLitNeedsParens prec ol
    go (HsPar{})                      = False
    go (HsApp{})                      = prec >= appPrec
    go (HsAppType {})                 = prec >= appPrec
    go (OpApp{})                      = prec >= opPrec
    go (NegApp{})                     = prec > topPrec
    go (SectionL{})                   = True
    go (SectionR{})                   = True
    -- Special-case unary boxed tuple applications so that they are
    -- parenthesized as `Identity (Solo x)`, not `Identity Solo x` (#18612)
    -- See Note [One-tuples] in GHC.Builtin.Types
    go (ExplicitTuple _ [Present{}] Boxed)
                                      = prec >= appPrec
    go (ExplicitTuple{})              = False
    go (ExplicitSum{})                = False
    go (HsLam{})                      = prec > topPrec
    go (HsCase{})                     = prec > topPrec
    go (HsIf{})                       = prec > topPrec
    go (HsMultiIf{})                  = prec > topPrec
    go (HsLet{})                      = prec > topPrec
    go (HsDo _ sc _)
      | isDoComprehensionContext sc   = False
      | otherwise                     = prec > topPrec
    go (ExplicitList{})               = False
    go (RecordUpd{})                  = False
    go (ExprWithTySig{})              = prec >= sigPrec
    go (ArithSeq{})                   = False
    go (HsPragE{})                    = prec >= appPrec
    go (HsTypedSplice{})              = False
    go (HsUntypedSplice{})            = False
    go (HsTypedBracket{})             = False
    go (HsUntypedBracket{})           = False
    go (HsProc{})                     = prec > topPrec
    go (HsStatic{})                   = prec >= appPrec
    go (RecordCon{})                  = False
    go (HsProjection{})               = True
    go (HsGetField{})                 = False
    go (HsEmbTy{})                    = prec > topPrec
    go (HsHole{})                     = False
    go (HsForAll{})                   = prec >= funPrec
    go (HsQual{})                     = prec >= funPrec
    go (HsFunArr{})                   = prec >= funPrec
    go (XExpr x) = case ghcPass @p of
                     GhcTc -> go_x_tc x
                     GhcRn -> go_x_rn x

    go_x_tc :: XXExprGhcTc -> Bool
    go_x_tc (WrapExpr _ e)                   = hsExprNeedsParens prec e
    go_x_tc (ExpandedThingTc thing _)        = hsExpandedNeedsParens thing
    go_x_tc (ConLikeTc {})                   = False
    go_x_tc (HsTick _ (L _ e))               = hsExprNeedsParens prec e
    go_x_tc (HsBinTick _ _ (L _ e))          = hsExprNeedsParens prec e
    go_x_tc (HsRecSelTc{})                   = False

    go_x_rn :: XXExprGhcRn -> Bool
    go_x_rn (ExpandedThingRn thing _)    = hsExpandedNeedsParens thing
    go_x_rn (PopErrCtxt (L _ a))         = hsExprNeedsParens prec a
    go_x_rn (HsRecSelRn{})               = False

    hsExpandedNeedsParens :: HsThingRn -> Bool
    hsExpandedNeedsParens (OrigExpr e) = hsExprNeedsParens prec e
    hsExpandedNeedsParens _            = False

-- | Parenthesize an expression without token information
gHsPar :: forall p. IsPass p => LHsExpr (GhcPass p) -> HsExpr (GhcPass p)
gHsPar e = HsPar x e
  where
    x = case ghcPass @p of
      GhcPs -> noAnn
      GhcRn -> noExtField
      GhcTc -> noExtField

-- | @'parenthesizeHsExpr' p e@ checks if @'hsExprNeedsParens' p e@ is true,
-- and if so, surrounds @e@ with an 'HsPar'. Otherwise, it simply returns @e@.
parenthesizeHsExpr :: IsPass p => PprPrec -> LHsExpr (GhcPass p) -> LHsExpr (GhcPass p)
parenthesizeHsExpr p le@(L loc e)
  | hsExprNeedsParens p e = L loc (gHsPar le)
  | otherwise             = le

stripParensLHsExpr :: LHsExpr (GhcPass p) -> LHsExpr (GhcPass p)
stripParensLHsExpr (L _ (HsPar _ e)) = stripParensLHsExpr e
stripParensLHsExpr e = e

stripParensHsExpr :: HsExpr (GhcPass p) -> HsExpr (GhcPass p)
stripParensHsExpr (HsPar _ (L _ e)) = stripParensHsExpr e
stripParensHsExpr e = e

isAtomicHsExpr :: forall p. IsPass p => HsExpr (GhcPass p) -> Bool
-- True of a single token
isAtomicHsExpr (HsVar {})        = True
isAtomicHsExpr (HsLit {})        = True
isAtomicHsExpr (HsOverLit {})    = True
isAtomicHsExpr (HsIPVar {})      = True
isAtomicHsExpr (HsOverLabel {})  = True
isAtomicHsExpr (HsHole{})        = True
isAtomicHsExpr (XExpr x)
  | GhcTc <- ghcPass @p          = go_x_tc x
  | GhcRn <- ghcPass @p          = go_x_rn x
  where
    go_x_tc :: XXExprGhcTc -> Bool
    go_x_tc (WrapExpr _ e)            = isAtomicHsExpr e
    go_x_tc (ExpandedThingTc thing _) = isAtomicExpandedThingRn thing
    go_x_tc (ConLikeTc {})            = True
    go_x_tc (HsTick {})               = False
    go_x_tc (HsBinTick {})            = False
    go_x_tc (HsRecSelTc{})            = True

    go_x_rn :: XXExprGhcRn -> Bool
    go_x_rn (ExpandedThingRn thing _) = isAtomicExpandedThingRn thing
    go_x_rn (PopErrCtxt (L _ a))      = isAtomicHsExpr a
    go_x_rn (HsRecSelRn{})            = True

    isAtomicExpandedThingRn :: HsThingRn -> Bool
    isAtomicExpandedThingRn (OrigExpr e) = isAtomicHsExpr e
    isAtomicExpandedThingRn _            = False

isAtomicHsExpr _ = False

instance Outputable (HsPragE (GhcPass p)) where
  ppr (HsPragSCC (_, st) (StringLiteral stl lbl _)) =
    pprWithSourceText st (text "{-# SCC")
     -- no doublequotes if stl empty, for the case where the SCC was written
     -- without quotes.
    <+> pprWithSourceText stl (ftext lbl) <+> text "#-}"


{- *********************************************************************
*                                                                      *
             XXExprGhcRn and rebindable syntax
*                                                                      *
********************************************************************* -}

{- Note [Rebindable syntax and XXExprGhcRn]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We implement rebindable syntax (RS) support by performing a desugaring
in the renamer. We transform GhcPs expressions and patterns affected by
RS into the appropriate desugared form, but **annotated with the original
expression/pattern**.

Let us consider a piece of code like:

    {-# LANGUAGE RebindableSyntax #-}
    ifThenElse :: Char -> () -> () -> ()
    ifThenElse _ _ _ = ()
    x = if 'a' then () else True

The parsed AST for the RHS of x would look something like (slightly simplified):

    L locif (HsIf (L loca 'a') (L loctrue ()) (L locfalse True))

Upon seeing such an AST with RS on, we could transform it into a
mere function call, as per the RS rules, equivalent to the
following function application:

    ifThenElse 'a' () True

which doesn't typecheck. But GHC would report an error about
not being able to match the third argument's type (Bool) with the
expected type: (), in the expression _as desugared_, i.e in
the aforementioned function application. But the user never
wrote a function application! This would be pretty bad.

To remedy this, instead of transforming the original HsIf
node into mere applications of 'ifThenElse', we keep the
original 'if' expression around too, using the TTG
XExpr extension point to allow GHC to construct an
'XXExprGhcRn' value that will keep track of the original
expression in its first field, and the desugared one in the
second field. The resulting renamed AST would look like:

    L locif (XExpr
      (ExpandedThingRn
        (HsIf (L loca 'a')
              (L loctrue ())
              (L locfalse True)
        )
        (App (L generatedSrcSpan
                (App (L generatedSrcSpan
                        (App (L generatedSrcSpan (Var ifThenElse))
                             (L loca 'a')
                        )
                     )
                     (L loctrue ())
                )
             )
             (L locfalse True)
        )
      )
    )

When comes the time to typecheck the program, we end up calling
tcMonoExpr on the AST above. If this expression gives rise to
a type error, then it will appear in a context line and GHC
will pretty-print it using the 'Outputable (XXExprGhcRn a b)'
instance defined below, which *only prints the original
expression*. This is the gist of the idea, but is not quite
enough to recover the error messages that we had with the
SyntaxExpr-based, typechecking/desugaring-to-core time
implementation of rebindable syntax. The key idea is to decorate
some elements of the desugared expression so as to be able to
give them a special treatment when typechecking the desugared
expression, to print a different context line or skip one
altogether.

Whenever we 'setSrcSpan' a 'generatedSrcSpan', we update a field in
TcLclEnv called 'tcl_in_gen_code', setting it to True, which indicates that we
entered generated code, i.e code fabricated by the compiler when rebinding some
syntax. If someone tries to push some error context line while that field is set
to True, the pushing won't actually happen and the context line is just dropped.
Once we 'setSrcSpan' a real span (for an expression that was in the original
source code), we set 'tcl_in_gen_code' back to False, indicating that we
"emerged from the generated code tunnel", and that the expressions we will be
processing are relevant to report in context lines again.

You might wonder why TcLclEnv has both
   tcl_loc         :: RealSrcSpan
   tcl_in_gen_code :: Bool
Could we not store a Maybe RealSrcSpan? The problem is that we still
generate constraints when processing generated code, and a CtLoc must
contain a RealSrcSpan -- otherwise, error messages might appear
without source locations. So tcl_loc keeps the RealSrcSpan of the last
location spotted that wasn't generated; it's as good as we're going to
get in generated code. Once we get to sub-trees that are not
generated, then we update the RealSrcSpan appropriately, and set the
tcl_in_gen_code Bool to False.

---

An overview of the constructs that are desugared in this way is laid out in
Note [Handling overloaded and rebindable constructs] in GHC.Rename.Expr.

A general recipe to follow this approach for new constructs could go as follows:

- Remove any GhcRn-time SyntaxExpr extensions to the relevant constructor for your
  construct, in HsExpr or related syntax data types.
- At renaming-time:
    - take your original node of interest (HsIf above)
    - rename its subexpressions/subpatterns (condition and true/false
      branches above)
    - construct the suitable "rebound"-and-renamed result (ifThenElse call
      above), where the 'SrcSpan' attached to any _fabricated node_ (the
      HsVar/HsApp nodes, above) is set to 'generatedSrcSpan'
    - take both the original node and that rebound-and-renamed result and wrap
      them into an expansion construct:
        for expressions, XExpr (ExpandedThingRn <original node> <desugared>)
        for patterns, XPat (HsPatExpanded <original node> <desugared>)
 - At typechecking-time:
    - remove any logic that was previously dealing with your rebindable
      construct, typically involving [tc]SyntaxOp, SyntaxExpr and friends.
    - the XExpr (ExpandedThingRn ... ...) case in tcExpr already makes sure that we
      typecheck the desugared expression while reporting the original one in
      errors
-}

{- Note [Overview of record dot syntax]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This is the note that explains all the moving parts for record dot
syntax.

The language extensions @OverloadedRecordDot@ and
@OverloadedRecordUpdate@ (providing "record dot syntax") are
implemented using the techniques of Note [Rebindable syntax and
XXExprGhcRn].

When OverloadedRecordDot is enabled:
- Field selection expressions
  - e.g. foo.bar.baz
  - Have abstract syntax HsGetField
  - After renaming are XExpr (ExpandedThingRn (HsGetField ...) (getField @"..."...)) expressions
- Field selector expressions e.g. (.x.y)
  - Have abstract syntax HsProjection
  - After renaming are XExpr (ExpandedThingRn (HsProjection ...) ((getField @"...") . (getField @"...") . ...) expressions

When OverloadedRecordUpdate is enabled:
- Record update expressions
  - e.g. a{foo.bar=1, quux="corge", baz}
  - Have abstract syntax RecordUpd
    - With rupd_flds containting a Right
    - See Note [RecordDotSyntax field updates] (in Language.Haskell.Syntax.Expr)
  - After renaming are XExpr (ExpandedThingRn (RecordUpd ...) (setField@"..." ...) expressions
    - Note that this is true for all record updates even for those that do not involve '.'

When OverloadedRecordDot is enabled and RebindableSyntax is not
enabled the name 'getField' is resolved to GHC.Records.getField. When
OverloadedRecordDot is enabled and RebindableSyntax is enabled the
name 'getField' is whatever in-scope name that is.

When OverloadedRecordUpd is enabled and RebindableSyntax is not
enabled it is an error for now (temporary while we wait on native
setField support; see
https://gitlab.haskell.org/ghc/ghc/-/issues/16232). When
OverloadedRecordUpd is enabled and RebindableSyntax is enabled the
names 'getField' and 'setField' are whatever in-scope names they are.
-}


{-
************************************************************************
*                                                                      *
\subsection{Commands (in arrow abstractions)}
*                                                                      *
************************************************************************
-}

type instance XCmdArrApp  GhcPs = (IsUnicodeSyntax, EpaLocation)
type instance XCmdArrApp  GhcRn = NoExtField
type instance XCmdArrApp  GhcTc = Type

type instance XCmdArrForm GhcPs = AnnList ()
-- | fixity (filled in by the renamer), for forms that were converted from
-- OpApp's by the renamer
type instance XCmdArrForm GhcRn = Maybe Fixity
type instance XCmdArrForm GhcTc = Maybe Fixity

type instance XCmdApp     (GhcPass _) = NoExtField
type instance XCmdLam     (GhcPass _) = NoExtField

type instance XCmdPar     GhcPs = (EpToken "(", EpToken ")")
type instance XCmdPar     GhcRn = NoExtField
type instance XCmdPar     GhcTc = NoExtField

type instance XCmdCase    GhcPs = EpAnnHsCase
type instance XCmdCase    GhcRn = NoExtField
type instance XCmdCase    GhcTc = NoExtField

type instance XCmdLamCase (GhcPass _) = EpAnnLam

type instance XCmdIf      GhcPs = AnnsIf
type instance XCmdIf      GhcRn = NoExtField
type instance XCmdIf      GhcTc = NoExtField

type instance XCmdLet     GhcPs = (EpToken "let", EpToken "in")
type instance XCmdLet     GhcRn = NoExtField
type instance XCmdLet     GhcTc = NoExtField

type instance XCmdDo      GhcPs = AnnList EpaLocation
type instance XCmdDo      GhcRn = NoExtField
type instance XCmdDo      GhcTc = Type

type instance XCmdWrap    (GhcPass _) = NoExtField

type instance XXCmd       GhcPs = DataConCantHappen
type instance XXCmd       GhcRn = DataConCantHappen
type instance XXCmd       GhcTc = HsWrap HsCmd

    -- If   cmd :: arg1 --> res
    --      wrap :: arg1 "->" arg2
    -- Then (XCmd (HsWrap wrap cmd)) :: arg2 --> res

-- | Command Syntax Table (for Arrow syntax)
type CmdSyntaxTable p = [(Name, HsExpr p)]
-- See Note [CmdSyntaxTable]

{-
Note [CmdSyntaxTable]
~~~~~~~~~~~~~~~~~~~~~
Used only for arrow-syntax stuff (HsCmdTop), the CmdSyntaxTable keeps
track of the methods needed for a Cmd.

* Before the renamer, this list is an empty list

* After the renamer, it takes the form @[(std_name, HsVar actual_name)]@
  For example, for the 'arr' method
   * normal case:            (GHC.Control.Arrow.arr, HsVar GHC.Control.Arrow.arr)
   * with rebindable syntax: (GHC.Control.Arrow.arr, arr_22)
             where @arr_22@ is whatever 'arr' is in scope

* After the type checker, it takes the form [(std_name, <expression>)]
  where <expression> is the evidence for the method.  This evidence is
  instantiated with the class, but is still polymorphic in everything
  else.  For example, in the case of 'arr', the evidence has type
         forall b c. (b->c) -> a b c
  where 'a' is the ambient type of the arrow.  This polymorphism is
  important because the desugarer uses the same evidence at multiple
  different types.

This is Less Cool than what we normally do for rebindable syntax, which is to
make fully-instantiated piece of evidence at every use site.  The Cmd way
is Less Cool because
  * The renamer has to predict which methods are needed.
    See the tedious GHC.Rename.Expr.methodNamesCmd.

  * The desugarer has to know the polymorphic type of the instantiated
    method. This is checked by Inst.tcSyntaxName, but is less flexible
    than the rest of rebindable syntax, where the type is less
    pre-ordained.  (And this flexibility is useful; for example we can
    typecheck do-notation with (>>=) :: m1 a -> (a -> m2 b) -> m2 b.)
-}

data CmdTopTc
  = CmdTopTc Type    -- Nested tuple of inputs on the command's stack
             Type    -- return type of the command
             (CmdSyntaxTable GhcTc) -- See Note [CmdSyntaxTable]

type instance XCmdTop  GhcPs = NoExtField
type instance XCmdTop  GhcRn = CmdSyntaxTable GhcRn -- See Note [CmdSyntaxTable]
type instance XCmdTop  GhcTc = CmdTopTc


type instance XXCmdTop (GhcPass _) = DataConCantHappen

instance (OutputableBndrId p) => Outputable (HsCmd (GhcPass p)) where
    ppr cmd = pprCmd cmd

-----------------------
-- pprCmd and pprLCmd call pprDeeper;
-- the underscore versions do not
pprLCmd :: (OutputableBndrId p) => LHsCmd (GhcPass p) -> SDoc
pprLCmd (L _ c) = pprCmd c

pprCmd :: (OutputableBndrId p) => HsCmd (GhcPass p) -> SDoc
pprCmd c | isQuietHsCmd c =            ppr_cmd c
         | otherwise      = pprDeeper (ppr_cmd c)

isQuietHsCmd :: HsCmd id -> Bool
-- Parentheses do display something, but it gives little info and
-- if we go deeper when we go inside them then we get ugly things
-- like (...)
isQuietHsCmd (HsCmdPar {}) = True
-- applications don't display anything themselves
isQuietHsCmd (HsCmdApp {}) = True
isQuietHsCmd _ = False

-----------------------
ppr_lcmd :: (OutputableBndrId p) => LHsCmd (GhcPass p) -> SDoc
ppr_lcmd c = ppr_cmd (unLoc c)

ppr_cmd :: forall p. (OutputableBndrId p
                     ) => HsCmd (GhcPass p) -> SDoc
ppr_cmd (HsCmdPar _ c) = parens (ppr_lcmd c)

ppr_cmd (HsCmdApp _ c e)
  = let (fun, args) = collect_args c [e] in
    hang (ppr_lcmd fun) 2 (sep (map ppr args))
  where
    collect_args (L _ (HsCmdApp _ fun arg)) args = collect_args fun (arg:args)
    collect_args fun args = (fun, args)

ppr_cmd (HsCmdLam _ LamSingle matches)
  = pprMatches matches
ppr_cmd (HsCmdLam _ lam_variant matches)
  = sep [ lamCaseKeyword lam_variant, nest 2 (pprMatches matches) ]

ppr_cmd (HsCmdCase _ expr matches)
  = sep [ sep [text "case", nest 4 (ppr expr), text "of"],
          nest 2 (pprMatches matches) ]

ppr_cmd (HsCmdIf _ _ e ct ce)
  = sep [hsep [text "if", nest 2 (ppr e), text "then"],
         nest 4 (ppr ct),
         text "else",
         nest 4 (ppr ce)]

-- special case: let ... in let ...
ppr_cmd (HsCmdLet _ binds cmd@(L _ (HsCmdLet {})))
  = sep [hang (text "let") 2 (hsep [pprBinds binds, text "in"]),
         ppr_lcmd cmd]

ppr_cmd (HsCmdLet _ binds cmd)
  = sep [hang (text "let") 2 (pprBinds binds),
         hang (text "in")  2 (ppr cmd)]

ppr_cmd (HsCmdDo _ (L _ stmts))  = pprArrowExpr stmts

ppr_cmd (HsCmdArrApp _ arrow arg HsFirstOrderApp True)
  = hsep [ppr_lexpr arrow, larrowt, ppr_lexpr arg]
ppr_cmd (HsCmdArrApp _ arrow arg HsFirstOrderApp False)
  = hsep [ppr_lexpr arg, arrowt, ppr_lexpr arrow]
ppr_cmd (HsCmdArrApp _ arrow arg HsHigherOrderApp True)
  = hsep [ppr_lexpr arrow, larrowtt, ppr_lexpr arg]
ppr_cmd (HsCmdArrApp _ arrow arg HsHigherOrderApp False)
  = hsep [ppr_lexpr arg, arrowtt, ppr_lexpr arrow]

ppr_cmd (HsCmdArrForm rn_fix (L _ op) ps_fix args)
  | HsVar _ (L _ v) <- op
  = ppr_cmd_infix v
  | GhcTc <- ghcPass @p
  , XExpr (ConLikeTc c _ _) <- op
  = ppr_cmd_infix (conLikeName c)
  | otherwise
  = fall_through
  where
    fall_through = hang (text "(|" <+> ppr_expr op)
                      4 (sep (map (pprCmdArg.unLoc) args) <+> text "|)")

    ppr_cmd_infix :: OutputableBndr v => v -> SDoc
    ppr_cmd_infix v
      | [arg1, arg2] <- args
      , case ghcPass @p of
          GhcPs -> ps_fix == Infix
          GhcRn -> isJust rn_fix || ps_fix == Infix
          GhcTc -> isJust rn_fix || ps_fix == Infix
      = hang (pprCmdArg (unLoc arg1))
           4 (sep [ pprInfixOcc v, pprCmdArg (unLoc arg2)])
      | otherwise
      = fall_through

ppr_cmd (XCmd x) = case ghcPass @p of
  GhcTc -> case x of
    HsWrap w cmd -> pprHsWrapper w (\_ -> parens (ppr_cmd cmd))

pprCmdArg :: (OutputableBndrId p) => HsCmdTop (GhcPass p) -> SDoc
pprCmdArg (HsCmdTop _ cmd)
  = ppr_lcmd cmd

instance (OutputableBndrId p) => Outputable (HsCmdTop (GhcPass p)) where
    ppr = pprCmdArg

{-
************************************************************************
*                                                                      *
\subsection{@Match@, @GRHSs@, and @GRHS@ datatypes}
*                                                                      *
************************************************************************
-}

type instance XMG         GhcPs b = Origin
type instance XMG         GhcRn b = Origin -- See Note [Generated code and pattern-match checking]
type instance XMG         GhcTc b = MatchGroupTc

data MatchGroupTc
  = MatchGroupTc
       { mg_arg_tys :: [Scaled Type]  -- Types of the arguments, t1..tn
       , mg_res_ty  :: Type    -- Type of the result, tr
       , mg_origin  :: Origin  -- Origin (Generated vs FromSource)
       } deriving Data

type instance XXMatchGroup (GhcPass _) b = DataConCantHappen

type instance XCMatch (GhcPass _) b = NoExtField
type instance XXMatch (GhcPass _) b = DataConCantHappen

instance (OutputableBndrId pr, Outputable body)
            => Outputable (Match (GhcPass pr) body) where
  ppr = pprMatch

isEmptyMatchGroup :: MatchGroup (GhcPass p) body -> Bool
isEmptyMatchGroup (MG { mg_alts = ms }) = null $ unLoc ms

-- | Is there only one RHS in this list of matches?
isSingletonMatchGroup :: [LMatch (GhcPass p) body] -> Bool
isSingletonMatchGroup matches
  | [L _ match] <- matches
  , Match { m_grhss = GRHSs { grhssGRHSs = _ :| [] } } <- match
  = True
  | otherwise
  = False

matchGroupArity :: MatchGroup (GhcPass id) body -> Arity
-- This is called before type checking, when mg_arg_tys is not set
matchGroupArity MG { mg_alts = L _ [] } = 1 -- See Note [Empty mg_alts]
matchGroupArity MG { mg_alts = L _ (alt1 : _) } = count (isVisArgPat . unLoc) (hsLMatchPats alt1)

hsLMatchPats :: LMatch (GhcPass id) body -> [LPat (GhcPass id)]
hsLMatchPats (L _ (Match { m_pats = L _ pats })) = pats

isInfixMatch :: Match (GhcPass p) body -> Bool
isInfixMatch match = case m_ctxt match of
  FunRhs {mc_fixity = Infix} -> True
  _                          -> False

-- We keep the type checker happy by providing EpAnnComments.  They
-- can only be used if they follow a `where` keyword with no binds,
-- but in that case the comment is attached to the following parsed
-- item. So this can never be used in practice.
type instance XCGRHSs (GhcPass _) _ = EpAnnComments

type instance XXGRHSs (GhcPass _) _ = DataConCantHappen

data GrhsAnn
  = GrhsAnn {
      ga_vbar :: Maybe (EpToken "|"),
      ga_sep  :: Either (EpToken "=") TokRarrow -- ^ Match separator location, `=` or `->`
      } deriving (Data)

instance NoAnn GrhsAnn where
  noAnn = GrhsAnn Nothing noAnn

type instance XCGRHS (GhcPass _) _ = EpAnn GrhsAnn
                                   -- Location of matchSeparator
                                   -- TODO:AZ does this belong on the GRHS, or GRHSs?

type instance XXGRHS (GhcPass _) b = DataConCantHappen

pprMatches :: (OutputableBndrId idR, Outputable body)
           => MatchGroup (GhcPass idR) body -> SDoc
pprMatches MG { mg_alts = matches }
    = vcat (map pprMatch (map unLoc (unLoc matches)))
      -- Don't print the type; it's only a place-holder before typechecking

-- Exported to GHC.Hs.Binds, which can't see the defn of HsMatchContext
pprFunBind :: (OutputableBndrId idR)
           => MatchGroup (GhcPass idR) (LHsExpr (GhcPass idR)) -> SDoc
pprFunBind matches = pprMatches matches

-- Exported to GHC.Hs.Binds, which can't see the defn of HsMatchContext
pprPatBind :: forall bndr p . (OutputableBndrId bndr,
                               OutputableBndrId p)
           => LPat (GhcPass bndr) -> GRHSs (GhcPass p) (LHsExpr (GhcPass p)) -> SDoc
pprPatBind pat grhss
 = sep [ppr pat,
       nest 2 (pprGRHSs (PatBindRhs :: HsMatchContext Void) grhss)]

pprMatch :: (OutputableBndrId idR, Outputable body)
         => Match (GhcPass idR) body -> SDoc
pprMatch (Match { m_pats = L _ pats, m_ctxt = ctxt, m_grhss = grhss })
  = sep [ sep (herald : map (nest 2 . pprParendLPat appPrec) other_pats)
        , nest 2 (pprGRHSs ctxt grhss) ]
  where
    -- lam_cases_result: we don't simply return (empty, pats) to avoid
    -- introducing an additional `nest 2` via the empty herald
    lam_cases_result = case pats of
                          []     -> (empty, [])
                          (p:ps) -> (pprParendLPat appPrec p, ps)

    (herald, other_pats)
        = case ctxt of
            FunRhs {mc_fun=L _ fun, mc_fixity=fixity, mc_strictness=strictness}
                | SrcStrict <- strictness
                -> assert (null pats)     -- A strict variable binding
                   (char '!'<>pprPrefixOcc fun, pats)

                | Prefix <- fixity
                -> (pprPrefixOcc fun, pats) -- f x y z = e
                                            -- Not pprBndr; the AbsBinds will
                                            -- have printed the signature
                | otherwise
                -> case pats of
                     (p1:p2:rest)
                        | null rest -> (pp_infix, [])           -- x &&& y = e
                        | otherwise -> (parens pp_infix, rest)  -- (x &&& y) z = e
                        where
                          pp_infix = pprParendLPat opPrec p1
                                     <+> pprInfixOcc fun
                                     <+> pprParendLPat opPrec p2
                     _ -> pprPanic "pprMatch" (ppr ctxt $$ ppr pats)

            LamAlt LamSingle                       -> (char '\\', pats)
            ArrowMatchCtxt (ArrowLamAlt LamSingle) -> (char '\\', pats)
            LamAlt LamCases                        -> lam_cases_result
            ArrowMatchCtxt (ArrowLamAlt LamCases)  -> lam_cases_result

            ArrowMatchCtxt ProcExpr -> (text "proc", pats)

            _ -> case pats of
                   []    -> (empty, [])
                   [pat] -> (ppr pat, [])  -- No parens around the single pat in a case
                   _     -> pprPanic "pprMatch" (ppr ctxt $$ ppr pats)

pprGRHSs :: (OutputableBndrId idR, Outputable body)
         => HsMatchContext fn -> GRHSs (GhcPass idR) body -> SDoc
pprGRHSs ctxt (GRHSs _ grhss binds)
  = vcat (toList $ NE.map (pprGRHS ctxt . unLoc) grhss)
  -- Print the "where" even if the contents of the binds is empty. Only
  -- EmptyLocalBinds means no "where" keyword
 $$ ppUnless (eqEmptyLocalBinds binds)
      (text "where" $$ nest 4 (pprBinds binds))

pprGRHS :: (OutputableBndrId idR, Outputable body)
        => HsMatchContext fn -> GRHS (GhcPass idR) body -> SDoc
pprGRHS ctxt (GRHS _ [] body)
 =  pp_rhs ctxt body

pprGRHS ctxt (GRHS _ guards body)
 = sep [vbar <+> interpp'SP guards, pp_rhs ctxt body]

pp_rhs :: Outputable body => HsMatchContext fn -> body -> SDoc
pp_rhs ctxt rhs = matchSeparator ctxt <+> pprDeeper (ppr rhs)

matchSeparator :: HsMatchContext fn -> SDoc
matchSeparator FunRhs{}         = text "="
matchSeparator CaseAlt          = arrow
matchSeparator LamAlt{}         = arrow
matchSeparator IfAlt            = arrow
matchSeparator ArrowMatchCtxt{} = arrow
matchSeparator PatBindRhs       = text "="
matchSeparator PatBindGuards    = text "="
matchSeparator StmtCtxt{}       = text "<-"
matchSeparator RecUpd           = text "="  -- This can be printed by the pattern
matchSeparator PatSyn           = text "<-" -- match checker trace
matchSeparator LazyPatCtx       = panic "unused"
matchSeparator ThPatSplice      = panic "unused"
matchSeparator ThPatQuote       = panic "unused"

instance Outputable GrhsAnn where
  ppr (GrhsAnn v s) = text "GrhsAnn" <+> ppr v <+> ppr s

{-
************************************************************************
*                                                                      *
\subsection{Do stmts and list comprehensions}
*                                                                      *
************************************************************************
-}

-- Extra fields available post typechecking for RecStmt.
data RecStmtTc =
  RecStmtTc
     { recS_bind_ty :: Type       -- S in (>>=) :: Q -> (R -> S) -> T
     , recS_later_rets :: [PostTcExpr] -- (only used in the arrow version)
     , recS_rec_rets :: [PostTcExpr] -- These expressions correspond 1-to-1
                                  -- with recS_later_ids and recS_rec_ids,
                                  -- and are the expressions that should be
                                  -- returned by the recursion.
                                  -- They may not quite be the Ids themselves,
                                  -- because the Id may be *polymorphic*, but
                                  -- the returned thing has to be *monomorphic*,
                                  -- so they may be type applications

      , recS_ret_ty :: Type        -- The type of
                                   -- do { stmts; return (a,b,c) }
                                   -- With rebindable syntax the type might not
                                   -- be quite as simple as (m (tya, tyb, tyc)).
      }


type instance XLastStmt        (GhcPass _) (GhcPass _) b = NoExtField

type instance XBindStmt        (GhcPass _) GhcPs b = EpUniToken "<-" "←"
type instance XBindStmt        (GhcPass _) GhcRn b = XBindStmtRn
type instance XBindStmt        (GhcPass _) GhcTc b = XBindStmtTc

data XBindStmtRn = XBindStmtRn
  { xbsrn_bindOp :: SyntaxExpr GhcRn
  , xbsrn_failOp :: FailOperator GhcRn
  }

data XBindStmtTc = XBindStmtTc
  { xbstc_bindOp :: SyntaxExpr GhcTc
  , xbstc_boundResultType :: Type -- If (>>=) :: Q -> (R -> S) -> T, this is S
  , xbstc_boundResultMult :: Mult -- If (>>=) :: Q -> (R -> S) -> T, this is S
  , xbstc_failOp :: FailOperator GhcTc
  }

type instance XApplicativeStmt (GhcPass _) GhcPs = NoExtField
type instance XApplicativeStmt (GhcPass _) GhcRn = NoExtField
type instance XApplicativeStmt (GhcPass _) GhcTc = Type

type instance XBodyStmt        (GhcPass _) GhcPs b = NoExtField
type instance XBodyStmt        (GhcPass _) GhcRn b = NoExtField
type instance XBodyStmt        (GhcPass _) GhcTc b = Type

type instance XLetStmt         (GhcPass _) (GhcPass _) b = EpToken "let"

type instance XParStmt         (GhcPass _) GhcPs b = NoExtField
type instance XParStmt         (GhcPass _) GhcRn b = NoExtField
type instance XParStmt         (GhcPass _) GhcTc b = Type

type instance XTransStmt       (GhcPass _) GhcPs b = AnnTransStmt
type instance XTransStmt       (GhcPass _) GhcRn b = NoExtField
type instance XTransStmt       (GhcPass _) GhcTc b = Type

type instance XRecStmt         (GhcPass _) GhcPs b = AnnList (EpToken "rec")
type instance XRecStmt         (GhcPass _) GhcRn b = NoExtField
type instance XRecStmt         (GhcPass _) GhcTc b = RecStmtTc

type instance XXStmtLR         (GhcPass _) GhcPs b = DataConCantHappen
type instance XXStmtLR         (GhcPass x) GhcRn b = ApplicativeStmt (GhcPass x) GhcRn
type instance XXStmtLR         (GhcPass x) GhcTc b = ApplicativeStmt (GhcPass x) GhcTc

data AnnTransStmt
  = AnnTransStmt {
      ats_then  :: EpToken "then",
      ats_group :: Maybe (EpToken "group"),
      ats_by    :: Maybe (EpToken "by"),
      ats_using :: Maybe (EpToken "using")
      } deriving Data

instance NoAnn AnnTransStmt where
  noAnn = AnnTransStmt noAnn noAnn noAnn noAnn


-- | 'ApplicativeStmt' represents an applicative expression built with
-- '<$>' and '<*>'.  It is generated by the renamer, and is desugared into the
-- appropriate applicative expression by the desugarer, but it is intended
-- to be invisible in error messages.
--
-- For full details, see Note [ApplicativeDo] in "GHC.Rename.Expr"
--
data ApplicativeStmt idL idR
  = ApplicativeStmt
             (XApplicativeStmt idL idR) -- Post typecheck, Type of the body
             [ ( SyntaxExpr idR
               , ApplicativeArg idL) ]
                      -- [(<$>, e1), (<*>, e2), ..., (<*>, en)]
             (Maybe (SyntaxExpr idR))  -- 'join', if necessary

-- | Applicative Argument
data ApplicativeArg idL
  = ApplicativeArgOne      -- A single statement (BindStmt or BodyStmt)
    { xarg_app_arg_one  :: XApplicativeArgOne idL
      -- ^ The fail operator, after renaming
      --
      -- The fail operator is needed if this is a BindStmt
      -- where the pattern can fail. E.g.:
      -- (Just a) <- stmt
      -- The fail operator will be invoked if the pattern
      -- match fails.
      -- It is also used for guards in MonadComprehensions.
      -- The fail operator is Nothing
      -- if the pattern match can't fail
    , app_arg_pattern   :: LPat idL -- WildPat if it was a BodyStmt (see below)
    , arg_expr          :: LHsExpr idL
    , is_body_stmt      :: Bool
      -- ^ True <=> was a BodyStmt,
      -- False <=> was a BindStmt.
      -- See Note [Applicative BodyStmt]
    }
  | ApplicativeArgMany     -- do { stmts; return vars }
    { xarg_app_arg_many :: XApplicativeArgMany idL
    , app_stmts         :: [ExprLStmt idL] -- stmts
    , final_expr        :: HsExpr idL    -- return (v1,..,vn), or just (v1,..,vn)
    , bv_pattern        :: LPat idL      -- (v1,...,vn)
    , stmt_context      :: HsDoFlavour
      -- ^ context of the do expression, used in pprArg
    }
  | XApplicativeArg !(XXApplicativeArg idL)

type family XApplicativeStmt x x'

-- ApplicativeArg type families
type family XApplicativeArgOne   x
type family XApplicativeArgMany  x
type family XXApplicativeArg     x

type instance XParStmtBlock  (GhcPass pL) (GhcPass pR) = NoExtField
type instance XXParStmtBlock (GhcPass pL) (GhcPass pR) = DataConCantHappen

type instance XApplicativeArgOne GhcPs = NoExtField
type instance XApplicativeArgOne GhcRn = FailOperator GhcRn
type instance XApplicativeArgOne GhcTc = FailOperator GhcTc

type instance XApplicativeArgMany (GhcPass _) = NoExtField
type instance XXApplicativeArg    (GhcPass _) = DataConCantHappen

instance (Outputable (StmtLR (GhcPass idL) (GhcPass idL) (LHsExpr (GhcPass idL))),
          Outputable (XXParStmtBlock (GhcPass idL) (GhcPass idR)))
        => Outputable (ParStmtBlock (GhcPass idL) (GhcPass idR)) where
  ppr (ParStmtBlock _ stmts _ _) = interpp'SP stmts

instance (OutputableBndrId pl, OutputableBndrId pr,
                 Anno (StmtLR (GhcPass pl) (GhcPass pr) body) ~ SrcSpanAnnA,
          Outputable body)
         => Outputable (StmtLR (GhcPass pl) (GhcPass pr) body) where
    ppr stmt = pprStmt stmt

pprStmt :: forall idL idR body . (OutputableBndrId idL,
                                  OutputableBndrId idR,
                 Anno (StmtLR (GhcPass idL) (GhcPass idR) body) ~ SrcSpanAnnA,
                                  Outputable body)
        => (StmtLR (GhcPass idL) (GhcPass idR) body) -> SDoc
pprStmt (LastStmt _ expr m_dollar_stripped _)
  = whenPprDebug (text "[last]") <+>
      (case m_dollar_stripped of
        Just True -> text "return $"
        Just False -> text "return"
        Nothing -> empty) <+>
      ppr expr
pprStmt (BindStmt _ pat expr)  = pprBindStmt pat expr
pprStmt (LetStmt _ binds)      = hsep [text "let", pprBinds binds]
pprStmt (BodyStmt _ expr _ _)  = ppr expr
pprStmt (ParStmt _ stmtss _ _) = sep (punctuate (text " | ") (map ppr $ toList stmtss))

pprStmt (TransStmt { trS_stmts = stmts, trS_by = by
                   , trS_using = using, trS_form = form })
  = sep $ punctuate comma (map ppr stmts ++ [pprTransStmt by using form])

pprStmt (RecStmt { recS_stmts = segment, recS_rec_ids = rec_ids
                 , recS_later_ids = later_ids })
  = text "rec" <+>
    vcat [ ppr_do_stmts (unLoc segment)
         , whenPprDebug (vcat [ text "rec_ids=" <> ppr rec_ids
                            , text "later_ids=" <> ppr later_ids])]

pprStmt (XStmtLR x) = case ghcPass :: GhcPass idR of
    GhcRn -> pprApplicativeStmt x
    GhcTc -> pprApplicativeStmt x

  where
    pprApplicativeStmt :: (OutputableBndrId idL, OutputableBndrId idR) => ApplicativeStmt (GhcPass idL) (GhcPass idR) -> SDoc
    pprApplicativeStmt (ApplicativeStmt _ args mb_join) =
      getPprStyle $ \style ->
          if userStyle style
             then pp_for_user
             else pp_debug
      where
        -- make all the Applicative stuff invisible in error messages by
        -- flattening the whole ApplicativeStmt nest back to a sequence
        -- of statements.
        pp_for_user = vcat $ concatMap flattenArg args

        -- ppr directly rather than transforming here, because we need to
        -- inject a "return" which is hard when we're polymorphic in the id
        -- type.
        flattenStmt :: ExprLStmt (GhcPass idL) -> [SDoc]
        flattenStmt (L _ (XStmtLR x)) = case ghcPass :: GhcPass idL of
            GhcRn | (ApplicativeStmt _ args _) <- x -> concatMap flattenArg args
            GhcTc | (ApplicativeStmt _ args _) <- x -> concatMap flattenArg args
        flattenStmt stmt = [ppr stmt]

        flattenArg :: (a, ApplicativeArg (GhcPass idL)) -> [SDoc]
        flattenArg (_, ApplicativeArgOne _ pat expr isBody)
          | isBody =  [ppr expr] -- See Note [Applicative BodyStmt]
          | otherwise = [pprBindStmt pat expr]
        flattenArg (_, ApplicativeArgMany _ stmts _ _ _) =
          concatMap flattenStmt stmts

        pp_debug =
          let
              ap_expr = sep (punctuate (text " |") (map pp_arg args))
          in
            whenPprDebug (if isJust mb_join then text "[join]" else empty) <+>
            (if lengthAtLeast args 2 then parens else id) ap_expr

        pp_arg :: (a, ApplicativeArg (GhcPass idL)) -> SDoc
        pp_arg (_, applicativeArg) = ppr applicativeArg

pprBindStmt :: (Outputable pat, Outputable expr) => pat -> expr -> SDoc
pprBindStmt pat expr = hsep [ppr pat, larrow, ppr expr]

instance (OutputableBndrId idL)
      => Outputable (ApplicativeArg (GhcPass idL)) where
  ppr = pprArg

pprArg :: forall idL . (OutputableBndrId idL) => ApplicativeArg (GhcPass idL) -> SDoc
pprArg (ApplicativeArgOne _ pat expr isBody)
  | isBody = ppr expr -- See Note [Applicative BodyStmt]
  | otherwise = pprBindStmt pat expr
pprArg (ApplicativeArgMany _ stmts return pat ctxt) =
     ppr pat <+>
     text "<-" <+>
     pprDo ctxt (stmts ++
                   [noLocA (LastStmt noExtField (noLocA return) Nothing noSyntaxExpr)])

pprTransformStmt :: (OutputableBndrId p)
                 => [IdP (GhcPass p)] -> LHsExpr (GhcPass p)
                 -> Maybe (LHsExpr (GhcPass p)) -> SDoc
pprTransformStmt bndrs using by
  = sep [ text "then" <+> whenPprDebug (braces (ppr bndrs))
        , nest 2 (ppr using)
        , nest 2 (pprBy by)]

pprTransStmt :: Outputable body => Maybe body -> body -> TransForm -> SDoc
pprTransStmt by using ThenForm
  = sep [ text "then", nest 2 (ppr using), nest 2 (pprBy by)]
pprTransStmt by using GroupForm
  = sep [ text "then group", nest 2 (pprBy by), nest 2 (text "using" <+> ppr using)]

pprBy :: Outputable body => Maybe body -> SDoc
pprBy Nothing  = empty
pprBy (Just e) = text "by" <+> ppr e

pprDo :: (OutputableBndrId p, Outputable body,
                 Anno (StmtLR (GhcPass p) (GhcPass p) body) ~ SrcSpanAnnA
         )
      => HsDoFlavour -> [LStmt (GhcPass p) body] -> SDoc
pprDo (DoExpr m)    stmts =
  ppr_module_name_prefix m <> text "do"  <+> ppr_do_stmts stmts
pprDo GhciStmtCtxt  stmts = text "do"  <+> ppr_do_stmts stmts
pprDo (MDoExpr m)   stmts =
  ppr_module_name_prefix m <> text "mdo"  <+> ppr_do_stmts stmts
pprDo ListComp      stmts = brackets    $ pprComp stmts
pprDo MonadComp     stmts = brackets    $ pprComp stmts

pprArrowExpr :: (OutputableBndrId p, Outputable body,
                 Anno (StmtLR (GhcPass p) (GhcPass p) body) ~ SrcSpanAnnA
         )
      => [LStmt (GhcPass p) body] -> SDoc
pprArrowExpr stmts = text "do"  <+> ppr_do_stmts stmts

ppr_module_name_prefix :: Maybe ModuleName -> SDoc
ppr_module_name_prefix = \case
  Nothing -> empty
  Just module_name -> ppr module_name <> char '.'

ppr_do_stmts :: (OutputableBndrId idL, OutputableBndrId idR,
                 Anno (StmtLR (GhcPass idL) (GhcPass idR) body) ~ SrcSpanAnnA,
                 Outputable body)
             => [LStmtLR (GhcPass idL) (GhcPass idR) body] -> SDoc
-- Print a bunch of do stmts
ppr_do_stmts stmts = pprDeeperList vcat (map ppr stmts)

pprComp :: (OutputableBndrId p, Outputable body,
                 Anno (StmtLR (GhcPass p) (GhcPass p) body) ~ SrcSpanAnnA)
        => [LStmt (GhcPass p) body] -> SDoc
pprComp quals     -- Prints:  body | qual1, ..., qualn
  | Just (initStmts, L _ (LastStmt _ body _ _)) <- snocView quals
  = if null initStmts
       -- If there are no statements in a list comprehension besides the last
       -- one, we simply treat it like a normal list. This does arise
       -- occasionally in code that GHC generates, e.g., in implementations of
       -- 'range' for derived 'Ix' instances for product datatypes with exactly
       -- one constructor (e.g., see #12583).
       then ppr body
       else hang (ppr body <+> vbar) 2 (pprQuals initStmts)
  | otherwise
  = pprPanic "pprComp" (pprQuals quals)

pprQuals :: (OutputableBndrId p, Outputable body,
                 Anno (StmtLR (GhcPass p) (GhcPass p) body) ~ SrcSpanAnnA)
         => [LStmt (GhcPass p) body] -> SDoc
-- Show list comprehension qualifiers separated by commas
pprQuals quals = interpp'SP quals

{-
************************************************************************
*                                                                      *
                Template Haskell quotation brackets
*                                                                      *
************************************************************************
-}

-- | Finalizers produced by a splice with
-- 'Language.Haskell.TH.Syntax.addModFinalizer'
--
-- See Note [Delaying modFinalizers in untyped splices] in GHC.Rename.Splice. For how
-- this is used.
--
newtype ThModFinalizers = ThModFinalizers [ForeignRef (TH.Q ())]

-- A Data instance which ignores the argument of 'ThModFinalizers'.
instance Data ThModFinalizers where
  gunfold _ z _ = z $ ThModFinalizers []
  toConstr  a   = mkConstr (dataTypeOf a) "ThModFinalizers" [] Data.Prefix
  dataTypeOf a  = mkDataType "HsExpr.ThModFinalizers" [toConstr a]

-- See Note [Delaying modFinalizers in untyped splices] in GHC.Rename.Splice.
-- This is the result of splicing a splice. It is produced by
-- the renamer and consumed by the typechecker. It lives only between the two.
data HsUntypedSpliceResult thing  -- 'thing' can be HsExpr or HsType
  = HsUntypedSpliceTop
      { utsplice_result_finalizers :: ThModFinalizers -- ^ TH finalizers produced by the splice.
      , utsplice_result            :: thing           -- ^ The result of splicing; See Note [Lifecycle of a splice]
      }
  | HsUntypedSpliceNested SplicePointName -- A unique name to identify this splice point

-- See Note [Lifecycle of an untyped splice, and PendingRnSplice]
-- for an explanation of the Template Haskell extension points.
data HsTypedSpliceResult
  = HsTypedSpliceTop
  | HsTypedSpliceNested SplicePointName

type instance XTypedSplice   GhcPs = NoExtField
type instance XTypedSplice   GhcRn = HsTypedSpliceResult
type instance XTypedSplice   GhcTc = DelayedSplice

type instance XUntypedSplice GhcPs = NoExtField
type instance XUntypedSplice GhcRn = HsUntypedSpliceResult (HsExpr GhcRn)
type instance XUntypedSplice GhcTc = DataConCantHappen

-- HsUntypedSplice
type instance XUntypedSpliceExpr GhcPs = EpToken "$"
type instance XUntypedSpliceExpr GhcRn = HsUserSpliceExt
type instance XUntypedSpliceExpr GhcTc = DataConCantHappen

type instance XTypedSpliceExpr GhcPs = EpToken "$$"
type instance XTypedSpliceExpr GhcRn = NoExtField
type instance XTypedSpliceExpr GhcTc = NoExtField

type instance XQuasiQuote        GhcPs = NoExtField
type instance XQuasiQuote        GhcRn = HsQuasiQuoteExt
type instance XQuasiQuote        GhcTc = DataConCantHappen


type instance XXUntypedSplice    GhcPs = DataConCantHappen
type instance XXUntypedSplice    GhcRn = HsImplicitLiftSplice
type instance XXUntypedSplice    GhcTc = DataConCantHappen

type instance XXTypedSplice    GhcPs = DataConCantHappen
type instance XXTypedSplice    GhcRn = HsImplicitLiftSplice
type instance XXTypedSplice    GhcTc = DataConCantHappen

-- See Note [Running typed splices in the zonker]
-- These are the arguments that are passed to `GHC.Tc.Gen.Splice.runTopSplice`
data DelayedSplice =
  DelayedSplice
    TcLclEnv          -- The local environment to run the splice in
    (LHsExpr GhcRn)   -- The original renamed expression
    TcType            -- The result type of running the splice, unzonked
    (LHsExpr GhcTc)   -- The typechecked expression to run and splice in the result

-- A Data instance which ignores the argument of 'DelayedSplice'.
instance Data DelayedSplice where
  gunfold _ _ _ = panic "DelayedSplice"
  toConstr  a   = mkConstr (dataTypeOf a) "DelayedSplice" [] Data.Prefix
  dataTypeOf a  = mkDataType "HsExpr.DelayedSplice" [toConstr a]

-- See Note [Pending Splices]
type SplicePointName = Name

data UntypedSpliceFlavour
  = UntypedExpSplice
  | UntypedPatSplice
  | UntypedTypeSplice
  | UntypedDeclSplice
  deriving Data


-- See Note [Lifecycle of an untyped splice, and PendingRnSplice]
-- A 'PendingRnSplice' is lifted from an untyped quotation and then typechecked.
data PendingRnSplice = PendingRnSplice SplicePointName (HsUntypedSplice GhcRn)

instance Outputable PendingRnSplice where
  ppr (PendingRnSplice sp expr) =
    angleBrackets (ppr sp <> comma <+> pprUntypedSplice False Nothing expr)

-- | Pending Type-checker Splice
-- See Note [Lifecycle of an untyped splice, and PendingRnSplice]
data PendingTcSplice
  = PendingTcSplice SplicePointName (LHsExpr GhcTc)

-- | Information about an implicit lift, discovered by the renamer
-- See Note [Lifecycle of an untyped splice, and PendingRnSplice]
data HsImplicitLiftSplice =
        HsImplicitLiftSplice
          { implicit_lift_bind_lvl :: S.Set ThLevelIndex
          , implicit_lift_used_lvl :: ThLevelIndex
          , implicit_lift_gre :: Maybe GlobalRdrElt
          , implicit_lift_lid :: LIdOccP GhcRn
          }

-- | Information about a user-written splice, discovered by the renamer
-- See Note [Lifecycle of an untyped splice, and PendingRnSplice]
data HsUserSpliceExt =
  HsUserSpliceExt
    { user_splice_flavour :: UntypedSpliceFlavour
    }

-- | Information about a quasi-quoter, discovered by the renamer
-- See Note [Lifecycle of an untyped splice, and PendingRnSplice]
data HsQuasiQuoteExt =
  HsQuasiQuoteExt
    { quasi_quote_flavour :: UntypedSpliceFlavour
    }


pprTypedSplice :: forall p . (OutputableBndrId p) => Maybe SplicePointName -> HsTypedSplice (GhcPass p) -> SDoc
pprTypedSplice n (HsTypedSpliceExpr _ e) = ppr_splice (text "$$") n e
pprTypedSplice n (XTypedSplice p) =
  case ghcPass @p of
    GhcRn -> case p of
              HsImplicitLiftSplice _ _ _ lid -> ppr lid <+> whenPprDebug (maybe empty (brackets . ppr) n)

pprUntypedSplice :: forall p. (OutputableBndrId p)
                 => Bool -- Whether to precede the splice with "$"
                 -> Maybe SplicePointName -- Used for pretty printing when exists
                 -> HsUntypedSplice (GhcPass p)
                 -> SDoc
pprUntypedSplice True  n (HsUntypedSpliceExpr _ e) = ppr_splice (text "$") n e
pprUntypedSplice False n (HsUntypedSpliceExpr _ e) = ppr_splice empty n e
pprUntypedSplice _     _ (HsQuasiQuote _ q s)      = ppr_quasi (unLoc q) (unLoc s)
pprUntypedSplice _     _ (XUntypedSplice x) =
  case ghcPass @p of
    GhcRn -> case x of
              HsImplicitLiftSplice _ _ _ lid -> ppr lid

ppr_quasi :: OutputableBndr p => p -> FastString -> SDoc
ppr_quasi quoter quote = char '[' <> ppr quoter <> vbar <>
                           ppr quote <> text "|]"

ppr_splice :: (OutputableBndrId p)
           => SDoc
           -> Maybe SplicePointName
           -> LHsExpr (GhcPass p)
           -> SDoc
ppr_splice herald mn e
    = herald
    <> (case mn of
         Nothing -> empty
         Just splice_name -> whenPprDebug (brackets (ppr splice_name)))
    <> ppr e


type instance XExpBr  GhcPs       = (BracketAnn (EpUniToken "[|" "⟦") (EpToken "[e|"), EpUniToken "|]" "⟧")
type instance XPatBr  GhcPs       = (EpToken "[p|", EpUniToken "|]" "⟧")
type instance XDecBrL GhcPs       = (EpToken "[d|", EpUniToken "|]" "⟧", (EpToken "{", EpToken "}"))
type instance XDecBrG GhcPs       = NoExtField
type instance XTypBr  GhcPs       = (EpToken "[t|", EpUniToken "|]" "⟧")
type instance XVarBr  GhcPs       = EpaLocation
type instance XXQuote GhcPs       = DataConCantHappen

type instance XExpBr  GhcRn       = NoExtField
type instance XPatBr  GhcRn       = NoExtField
type instance XDecBrL GhcRn       = NoExtField
type instance XDecBrG GhcRn       = NoExtField
type instance XTypBr  GhcRn       = NoExtField
type instance XVarBr  GhcRn       = NoExtField
type instance XXQuote GhcRn       = DataConCantHappen

-- See Note [The life cycle of a TH quotation]
type instance XExpBr  GhcTc       = DataConCantHappen
type instance XPatBr  GhcTc       = DataConCantHappen
type instance XDecBrL GhcTc       = DataConCantHappen
type instance XDecBrG GhcTc       = DataConCantHappen
type instance XTypBr  GhcTc       = DataConCantHappen
type instance XVarBr  GhcTc       = DataConCantHappen
type instance XXQuote GhcTc       = NoExtField

instance OutputableBndrId p
          => Outputable (HsQuote (GhcPass p)) where
  ppr = pprHsQuote
    where
      pprHsQuote :: forall p. (OutputableBndrId p)
                   => HsQuote (GhcPass p) -> SDoc
      pprHsQuote (ExpBr _ e)   = thBrackets empty (ppr e)
      pprHsQuote (PatBr _ p)   = thBrackets (char 'p') (ppr p)
      pprHsQuote (DecBrG _ gp) = thBrackets (char 'd') (ppr gp)
      pprHsQuote (DecBrL _ ds) = thBrackets (char 'd') (vcat (map ppr ds))
      pprHsQuote (TypBr _ t)   = thBrackets (char 't') (ppr t)
      pprHsQuote (VarBr _ True n)
        = char '\'' <> pprPrefixOcc (unLoc n)
      pprHsQuote (VarBr _ False n)
        = text "''" <> pprPrefixOcc (unLoc n)
      pprHsQuote (XQuote b)  = case ghcPass @p of
          GhcTc -> pprPanic "pprHsQuote: `HsQuote GhcTc` shouldn't exist" (ppr b)
                   -- See Note [The life cycle of a TH quotation]

thBrackets :: SDoc -> SDoc -> SDoc
thBrackets pp_kind pp_body = char '[' <> pp_kind <> vbar <+>
                             pp_body <+> text "|]"

thTyBrackets :: SDoc -> SDoc
thTyBrackets pp_body = text "[||" <+> pp_body <+> text "||]"

instance Outputable PendingTcSplice where
  ppr (PendingTcSplice n e) = angleBrackets (ppr n <> comma <+> ppr (stripParensLHsExpr e))

ppr_with_pending_tc_splices :: SDoc -> [PendingTcSplice] -> SDoc
ppr_with_pending_tc_splices x [] = x
ppr_with_pending_tc_splices x ps = x $$ whenPprDebug (text "pending(tc)" <+> ppr ps)

{-
************************************************************************
*                                                                      *
\subsection{Enumerations and list comprehensions}
*                                                                      *
************************************************************************
-}

instance OutputableBndrId p
         => Outputable (ArithSeqInfo (GhcPass p)) where
    ppr (From e1)             = hcat [ppr e1, pp_dotdot]
    ppr (FromThen e1 e2)      = hcat [ppr e1, comma, space, ppr e2, pp_dotdot]
    ppr (FromTo e1 e3)        = hcat [ppr e1, pp_dotdot, ppr e3]
    ppr (FromThenTo e1 e2 e3)
      = hcat [ppr e1, comma, space, ppr e2, pp_dotdot, ppr e3]

pp_dotdot :: SDoc
pp_dotdot = text " .. "

{-
************************************************************************
*                                                                      *
\subsection{HsMatchCtxt}
*                                                                      *
************************************************************************
-}

type HsMatchContextPs = HsMatchContext (LIdP GhcPs)
type HsMatchContextRn = HsMatchContext (LIdP GhcRn)

type HsStmtContextRn = HsStmtContext (LIdP GhcRn)

instance Outputable fn => Outputable (HsMatchContext fn) where
  ppr m@(FunRhs{})            = text "FunRhs" <+> ppr (mc_fun m) <+> ppr (mc_fixity m)
  ppr CaseAlt                 = text "CaseAlt"
  ppr (LamAlt lam_variant)    = text "LamAlt" <+> ppr lam_variant
  ppr IfAlt                   = text "IfAlt"
  ppr (ArrowMatchCtxt c)      = text "ArrowMatchCtxt" <+> ppr c
  ppr PatBindRhs              = text "PatBindRhs"
  ppr PatBindGuards           = text "PatBindGuards"
  ppr RecUpd                  = text "RecUpd"
  ppr (StmtCtxt _)            = text "StmtCtxt _"
  ppr ThPatSplice             = text "ThPatSplice"
  ppr ThPatQuote              = text "ThPatQuote"
  ppr PatSyn                  = text "PatSyn"
  ppr LazyPatCtx              = text "LazyPatCtx"

instance Outputable HsLamVariant where
  ppr = text . \case
    LamSingle -> "LamSingle"
    LamCase   -> "LamCase"
    LamCases  -> "LamCases"

lamCaseKeyword :: HsLamVariant -> SDoc
lamCaseKeyword LamSingle = text "lambda"
lamCaseKeyword LamCase   = text "\\case"
lamCaseKeyword LamCases  = text "\\cases"

pprExternalSrcLoc :: (StringLiteral,(Int,Int),(Int,Int)) -> SDoc
pprExternalSrcLoc (StringLiteral _ src _,(n1,n2),(n3,n4))
  = ppr (src,(n1,n2),(n3,n4))

instance Outputable HsArrowMatchContext where
  ppr ProcExpr                  = text "ProcExpr"
  ppr ArrowCaseAlt              = text "ArrowCaseAlt"
  ppr (ArrowLamAlt lam_variant) = parens $ text "ArrowLamCaseAlt" <+> ppr lam_variant

pprHsArrType :: HsArrAppType -> SDoc
pprHsArrType HsHigherOrderApp = text "higher order arrow application"
pprHsArrType HsFirstOrderApp  = text "first order arrow application"

-----------------

instance Outputable fn => Outputable (HsStmtContext fn) where
    ppr = pprStmtContext

-- Used to generate the string for a *runtime* error message
matchContextErrString :: Outputable fn => HsMatchContext fn -> SDoc
matchContextErrString (FunRhs{mc_fun=fun})          = text "function" <+> ppr fun
matchContextErrString CaseAlt                       = text "case"
matchContextErrString (LamAlt lam_variant)          = lamCaseKeyword lam_variant
matchContextErrString IfAlt                         = text "multi-way if"
matchContextErrString PatBindRhs                    = text "pattern binding"
matchContextErrString PatBindGuards                 = text "pattern binding guards"
matchContextErrString RecUpd                        = text "record update"
matchContextErrString (ArrowMatchCtxt c)            = matchArrowContextErrString c
matchContextErrString ThPatSplice                   = panic "matchContextErrString"  -- Not used at runtime
matchContextErrString ThPatQuote                    = panic "matchContextErrString"  -- Not used at runtime
matchContextErrString PatSyn                        = text "pattern synonym"
matchContextErrString (StmtCtxt (ParStmtCtxt c))    = matchContextErrString (StmtCtxt c)
matchContextErrString (StmtCtxt (TransStmtCtxt c))  = matchContextErrString (StmtCtxt c)
matchContextErrString (StmtCtxt (PatGuard _))       = text "pattern guard"
matchContextErrString (StmtCtxt (ArrowExpr))        = text "'do' block"
matchContextErrString (StmtCtxt (HsDoStmt flavour)) = matchDoContextErrString flavour
matchContextErrString LazyPatCtx                    = text "irrefutable pattern"

matchArrowContextErrString :: HsArrowMatchContext -> SDoc
matchArrowContextErrString ProcExpr                  = text "proc"
matchArrowContextErrString ArrowCaseAlt              = text "case"
matchArrowContextErrString (ArrowLamAlt LamSingle)   = text "kappa"
matchArrowContextErrString (ArrowLamAlt lam_variant) = lamCaseKeyword lam_variant

matchDoContextErrString :: HsDoFlavour -> SDoc
matchDoContextErrString GhciStmtCtxt = text "interactive GHCi command"
matchDoContextErrString (DoExpr m)   = prependQualified m (text "'do' block")
matchDoContextErrString (MDoExpr m)  = prependQualified m (text "'mdo' block")
matchDoContextErrString ListComp     = text "list comprehension"
matchDoContextErrString MonadComp    = text "monad comprehension"

pprMatchInCtxt :: (OutputableBndrId idR, Outputable body)
               => Match (GhcPass idR) body -> SDoc
pprMatchInCtxt match  = hang (text "In" <+> pprMatchContext (m_ctxt match)
                                        <> colon)
                             4 (pprMatch match)

pprStmtInCtxt :: (OutputableBndrId idL,
                  OutputableBndrId idR,
                  Outputable fn,
                  Outputable body,
                 Anno (StmtLR (GhcPass idL) (GhcPass idR) body) ~ SrcSpanAnnA)
              => HsStmtContext fn
              -> StmtLR (GhcPass idL) (GhcPass idR) body
              -> SDoc
pprStmtInCtxt ctxt (LastStmt _ e _ _)
  | isComprehensionContext ctxt      -- For [ e | .. ], do not mutter about "stmts"
  = hang (text "In the expression:") 2 (ppr e)

pprStmtInCtxt ctxt stmt
  = hang (text "In a stmt of" <+> pprAStmtContext ctxt <> colon)
       2 (ppr_stmt stmt)
  where
    -- For Group and Transform Stmts, don't print the nested stmts!
    ppr_stmt (TransStmt { trS_by = by, trS_using = using
                        , trS_form = form }) = pprTransStmt by using form
    ppr_stmt stmt = pprStmt stmt


pprMatchContext :: Outputable p => HsMatchContext p -> SDoc
pprMatchContext ctxt
  | want_an ctxt = text "an" <+> pprMatchContextNoun ctxt
  | otherwise    = text "a"  <+> pprMatchContextNoun ctxt
  where
    want_an (FunRhs {})                              = True  -- Use "an" in front
    want_an (ArrowMatchCtxt ProcExpr)                = True
    want_an (ArrowMatchCtxt (ArrowLamAlt LamSingle)) = True
    want_an LazyPatCtx                               = True
    want_an _                                        = False

pprMatchContextNoun :: Outputable fn => HsMatchContext fn -> SDoc
pprMatchContextNoun (FunRhs {mc_fun=fun})   = text "equation for" <+> quotes (ppr fun)
pprMatchContextNoun CaseAlt                 = text "case alternative"
pprMatchContextNoun (LamAlt LamSingle)      = text "lambda abstraction"
pprMatchContextNoun (LamAlt lam_variant)    = lamCaseKeyword lam_variant
                                              <+> text "alternative"
pprMatchContextNoun IfAlt                   = text "multi-way if alternative"
pprMatchContextNoun RecUpd                  = text "record update"
pprMatchContextNoun ThPatSplice             = text "Template Haskell pattern splice"
pprMatchContextNoun ThPatQuote              = text "Template Haskell pattern quotation"
pprMatchContextNoun PatBindRhs              = text "pattern binding"
pprMatchContextNoun PatBindGuards           = text "pattern binding guards"
pprMatchContextNoun (ArrowMatchCtxt c)      = pprArrowMatchContextNoun c
pprMatchContextNoun (StmtCtxt ctxt)         = text "pattern binding in"
                                              $$ pprAStmtContext ctxt
pprMatchContextNoun PatSyn                  = text "pattern synonym declaration"
pprMatchContextNoun LazyPatCtx              = text "irrefutable pattern"

pprMatchContextNouns :: Outputable fn => HsMatchContext fn -> SDoc
pprMatchContextNouns (FunRhs {mc_fun=fun})   = text "equations for" <+> quotes (ppr fun)
pprMatchContextNouns PatBindGuards           = text "pattern binding guards"
pprMatchContextNouns (ArrowMatchCtxt c)      = pprArrowMatchContextNouns c
pprMatchContextNouns (StmtCtxt ctxt)         = text "pattern bindings in"
                                               $$ pprAStmtContext ctxt
pprMatchContextNouns ctxt                    = pprMatchContextNoun ctxt <> char 's'

pprArrowMatchContextNoun :: HsArrowMatchContext -> SDoc
pprArrowMatchContextNoun ProcExpr                     = text "arrow proc pattern"
pprArrowMatchContextNoun ArrowCaseAlt                 = text "case alternative within arrow notation"
pprArrowMatchContextNoun (ArrowLamAlt LamSingle)   = text "arrow kappa abstraction"
pprArrowMatchContextNoun (ArrowLamAlt lam_variant) = lamCaseKeyword lam_variant
                                                     <+> text "alternative within arrow notation"

pprArrowMatchContextNouns :: HsArrowMatchContext -> SDoc
pprArrowMatchContextNouns ArrowCaseAlt              = text "case alternatives within arrow notation"
pprArrowMatchContextNouns (ArrowLamAlt LamSingle)   = text "arrow kappa abstractions"
pprArrowMatchContextNouns (ArrowLamAlt lam_variant) = lamCaseKeyword lam_variant
                                                      <+> text "alternatives within arrow notation"
pprArrowMatchContextNouns ctxt                      = pprArrowMatchContextNoun ctxt <> char 's'

-----------------
pprAStmtContext, pprStmtContext :: Outputable fn => HsStmtContext fn -> SDoc
pprAStmtContext (HsDoStmt flavour) = pprAHsDoFlavour flavour
pprAStmtContext ctxt = text "a" <+> pprStmtContext ctxt

-----------------
pprStmtContext (HsDoStmt flavour) = pprHsDoFlavour flavour
pprStmtContext (PatGuard ctxt) = text "pattern guard for" $$ pprMatchContext ctxt
pprStmtContext ArrowExpr       = text "'do' block in an arrow command"

-- Drop the inner contexts when reporting errors, else we get
--     Unexpected transform statement
--     in a transformed branch of
--          transformed branch of
--          transformed branch of monad comprehension
pprStmtContext (ParStmtCtxt c) =
  ifPprDebug (sep [text "parallel branch of", pprAStmtContext c])
             (pprStmtContext c)
pprStmtContext (TransStmtCtxt c) =
  ifPprDebug (sep [text "transformed branch of", pprAStmtContext c])
             (pprStmtContext c)

pprStmtCat :: forall p body . IsPass p => Stmt (GhcPass p) body -> SDoc
pprStmtCat (TransStmt {})       = text "transform"
pprStmtCat (LastStmt {})        = text "return expression"
pprStmtCat (BodyStmt {})        = text "body"
pprStmtCat (BindStmt {})        = text "binding"
pprStmtCat (LetStmt {})         = text "let"
pprStmtCat (RecStmt {})         = text "rec"
pprStmtCat (ParStmt {})         = text "parallel"
pprStmtCat (XStmtLR _)          = text "applicative"

pprAHsDoFlavour, pprHsDoFlavour :: HsDoFlavour -> SDoc
pprAHsDoFlavour flavour = article <+> pprHsDoFlavour flavour
  where
    pp_an = text "an"
    pp_a  = text "a"
    article = case flavour of
                  MDoExpr Nothing -> pp_an
                  GhciStmtCtxt  -> pp_an
                  _             -> pp_a
pprHsDoFlavour (DoExpr m)      = prependQualified m (text "'do' block")
pprHsDoFlavour (MDoExpr m)     = prependQualified m (text "'mdo' block")
pprHsDoFlavour ListComp        = text "list comprehension"
pprHsDoFlavour MonadComp       = text "monad comprehension"
pprHsDoFlavour GhciStmtCtxt    = text "interactive GHCi command"

prependQualified :: Maybe ModuleName -> SDoc -> SDoc
prependQualified Nothing  t = t
prependQualified (Just _) t = text "qualified" <+> t

{-
************************************************************************
*                                                                      *
FieldLabelStrings
*                                                                      *
************************************************************************
-}

instance (UnXRec p, Outputable (XRec p FieldLabelString)) => Outputable (FieldLabelStrings p) where
  ppr (FieldLabelStrings flds) =
    hcat (punctuate dot (toList $ NE.map (ppr . unXRec @p) flds))

instance (UnXRec p, Outputable (XRec p FieldLabelString)) => OutputableBndr (FieldLabelStrings p) where
  pprInfixOcc = pprFieldLabelStrings
  pprPrefixOcc = pprFieldLabelStrings

instance (UnXRec p,  Outputable (XRec p FieldLabelString)) => OutputableBndr (Located (FieldLabelStrings p)) where
  pprInfixOcc = pprInfixOcc . unLoc
  pprPrefixOcc = pprInfixOcc . unLoc

pprFieldLabelStrings :: forall p. (UnXRec p, Outputable (XRec p FieldLabelString)) => FieldLabelStrings p -> SDoc
pprFieldLabelStrings (FieldLabelStrings flds) =
    hcat (punctuate dot (toList $ NE.map (ppr . unXRec @p) flds))

pprPrefixFastString :: FastString -> SDoc
pprPrefixFastString fs = pprPrefixOcc (mkVarUnqual fs)

instance UnXRec p => Outputable (DotFieldOcc p) where
  ppr (DotFieldOcc _ s) = (pprPrefixFastString . field_label . unXRec @p) s
  ppr XDotFieldOcc{} = text "XDotFieldOcc"

{-
************************************************************************
*                                                                      *
\subsection{Anno instances}
*                                                                      *
************************************************************************
-}

type instance Anno (HsExpr (GhcPass p)) = SrcSpanAnnA
type instance Anno [LocatedA (HsExpr (GhcPass p))] = SrcSpanAnnC
type instance Anno [LocatedA (StmtLR (GhcPass pl) (GhcPass pr) (LocatedA (HsExpr (GhcPass pr))))] = SrcSpanAnnLW
type instance Anno [LocatedA (StmtLR (GhcPass pl) (GhcPass pr) (LocatedA (HsCmd (GhcPass pr))))] = SrcSpanAnnLW

type instance Anno (HsCmd (GhcPass p)) = SrcSpanAnnA

type instance Anno (HsCmdTop (GhcPass p)) = EpAnnCO
type instance Anno [LocatedA (Match (GhcPass p) (LocatedA (HsExpr (GhcPass p))))] = SrcSpanAnnLW
type instance Anno [LocatedA (Match (GhcPass p) (LocatedA (HsCmd  (GhcPass p))))] = SrcSpanAnnLW
type instance Anno (Match (GhcPass p) (LocatedA (HsExpr (GhcPass p)))) = SrcSpanAnnA
type instance Anno (Match (GhcPass p) (LocatedA (HsCmd  (GhcPass p)))) = SrcSpanAnnA
type instance Anno [LocatedA (Pat (GhcPass p))] = EpaLocation
type instance Anno (GRHS (GhcPass p) (LocatedA (HsExpr (GhcPass p)))) = EpAnnCO
type instance Anno (GRHS (GhcPass p) (LocatedA (HsCmd  (GhcPass p)))) = EpAnnCO
type instance Anno (StmtLR (GhcPass pl) (GhcPass pr) (LocatedA (body (GhcPass pr)))) = SrcSpanAnnA

type instance Anno (HsUntypedSplice (GhcPass p)) = SrcSpanAnnA

type instance Anno [LocatedA (StmtLR (GhcPass pl) (GhcPass pr) (LocatedA (body (GhcPass pr))))] = SrcSpanAnnLW

type instance Anno (FieldLabelStrings (GhcPass p)) = EpAnnCO
type instance Anno FieldLabelString                = SrcSpanAnnN

type instance Anno FastString                      = EpAnnCO
  -- Used in HsQuasiQuote and perhaps elsewhere

type instance Anno (DotFieldOcc (GhcPass p))       = EpAnnCO

instance (HasAnnotation (Anno a))
   => WrapXRec (GhcPass p) a where
  wrapXRec = noLocA
