{-# LANGUAGE UndecidableInstances #-}

module GHC.Tc.Types.ErrCtxt
  ( ErrCtxt (..), HsCtxt(..), CodeSrcFlag (..)
  , UserSigType(..), FunAppCtxtFunArg(..)
  , TyConInstFlavour(..)

  -- * UserTypeCtxt
  , UserTypeCtxt(..), pprUserTypeCtxt, isSigMaybe
  , ReportRedundantConstraints(..), reportRedundantConstraints
  , redundantConstraintsSpan,
  )
  where

import GHC.Prelude
import {-# SOURCE #-} GHC.Hs.Expr (SplicePointName, HsMatchContextRn, HsStmtContextRn)
import {-# SOURCE #-} GHC.Hs.Expr () -- for outputable instances
import GHC.Hs.Type () -- for outputable instances
import GHC.Hs.Extension

import GHC.Parser.Annotation ( LocatedN, SrcSpanAnnA )

import GHC.Tc.Errors.Types.PromotionErr ( TermLevelUseCtxt )
import {-# SOURCE #-} GHC.Tc.Types.Origin   ( CtOrigin, ExpectedFunTyCtxt )
import GHC.Tc.Utils.TcType   ( TcType, TcTyCon, ExpType )

import GHC.Types.Basic       ( TyConFlavour )
import GHC.Types.Name        ( Name )
import GHC.Types.SrcLoc      ( SrcSpan, noSrcSpan )
import GHC.Types.Var         ( Id, TyCoVar )

import GHC.Unit.Types ( Module, InstantiatedModule )

import GHC.Core.Class    ( Class )
import GHC.Core.ConLike  ( ConLike )
import GHC.Core.PatSyn   ( PatSyn )
import GHC.Core.TyCon    ( TyCon )
import GHC.Core.TyCo.Rep ( Type, ThetaType, PredType )

import {-# SOURCE #-} GHC.Unit.State ( UnitState ) -- Break the module graph cycle for accesing HsCtxt in GHC.Hs.Expr

import GHC.Data.FastString  ( FastString )
import GHC.Utils.Outputable

import Language.Haskell.Syntax
import Language.Haskell.Syntax.Basic ( FieldLabelString(..) )
import GHC.Boot.TH.Syntax qualified as TH

import qualified Data.List.NonEmpty as NE

{- *********************************************************************
*                                                                      *
          UserTypeCtxt
*                                                                      *
********************************************************************* -}

-------------------------------------
-- | UserTypeCtxt describes the origin of the polymorphic type
-- in the places where we need an expression to have that type
data UserTypeCtxt
  = FunSigCtxt      -- Function type signature, when checking the type
                    -- Also used for types in SPECIALISE pragmas
       Name              -- Name of the function
       ReportRedundantConstraints
         -- See Note [Tracking needed EvIds] in GHC.Tc.Solver
         -- This field is usually 'WantRCC', but 'NoRCC' for
         --   * Record selectors (not important here)
         --   * Class and instance methods.  Here the code may legitimately
         --     be more polymorphic than the signature generated from the
         --     class declaration
         --   * Functions whose type signature has hidden the constraints
         --     behind a type synonym.  E.g.
         --          type Foo = forall a. Eq a => a -> a
         --          id :: Foo
         --          id x = x
         --     Here we can't give a good location for the redundant constraints
         --     (see lhsSigWcTypeContextSpan), so we don't report redundant
         --     constraints at all. It's not clear that this a good choice;
         --     perhaps we should report, just with a less informative SrcSpan.
         --     c.f. #16154

  | InfSigCtxt Name     -- Inferred type for function
  | ExprSigCtxt         -- Expression type signature
      ReportRedundantConstraints
  | KindSigCtxt         -- Kind signature
  | StandaloneKindSigCtxt  -- Standalone kind signature
       Name                -- Name of the type/class
  | TypeAppCtxt         -- Visible type application
  | ConArgCtxt Name     -- Data constructor argument
  | TySynCtxt Name      -- RHS of a type synonym decl
  | PatSynCtxt Name     -- Type sig for a pattern synonym
  | PatSigCtxt          -- Type sig in pattern
                        --   eg  f (x::t) = ...
                        --   or  (x::t, y) = e
  | ForSigCtxt Name     -- Foreign import or export signature
  | DefaultDeclCtxt     -- Class or types in a default declaration
  | InstDeclCtxt Bool   -- An instance declaration
                        --    True:  stand-alone deriving
                        --    False: vanilla instance declaration
  | SpecInstCtxt        -- SPECIALISE instance pragma
  | GenSigCtxt          -- Higher-rank or impredicative situations
                        -- e.g. (f e) where f has a higher-rank type
                        -- We might want to elaborate this
  | GhciCtxt Bool       -- GHCi command :kind <type>
                        -- The Bool indicates if we are checking the outermost
                        -- type application.
                        -- See Note [Unsaturated type synonyms in GHCi] in
                        -- GHC.Tc.Validity.

  | ClassSCCtxt Name    -- Superclasses of a class
  | SigmaCtxt           -- Theta part of a normal for-all type
                        --      f :: <S> => a -> a
  | DataTyCtxt Name     -- The "stupid theta" part of a data decl
                        --      data <S> => T a = MkT a
  | DerivClauseCtxt     -- A 'deriving' clause
  | TyVarBndrKindCtxt Name  -- The kind of a type variable being bound
  | RuleBndrTypeCtxt Name   -- The type of a term variable being bound in a RULE
                            -- or SPECIALISE pragma
                            --    RULE "foo" forall (x :: a -> a). f (Just x) = ...
  | DataKindCtxt Name   -- The kind of a data/newtype (instance)
  | TySynKindCtxt Name  -- The kind of the RHS of a type synonym
  | TyFamResKindCtxt Name   -- The result kind of a type family
  deriving( Eq ) -- Just for checkSkolInfoAnon

-- | Report Redundant Constraints.
data ReportRedundantConstraints
  = NoRRC            -- ^ Don't report redundant constraints

  | WantRRC SrcSpan  -- ^ Report redundant constraints
      -- The SrcSpan is for the constraints
      -- E.g. f :: (Eq a, Ord b) => blah
      --      The span is for the (Eq a, Ord b)
      -- We need to record the span here because we have
      -- long since discarded the HsType in favour of a Type

  deriving( Eq )  -- Just for checkSkolInfoAnon

reportRedundantConstraints :: ReportRedundantConstraints -> Bool
reportRedundantConstraints NoRRC        = False
reportRedundantConstraints (WantRRC {}) = True

redundantConstraintsSpan :: UserTypeCtxt -> SrcSpan
redundantConstraintsSpan (FunSigCtxt _ (WantRRC span)) = span
redundantConstraintsSpan (ExprSigCtxt (WantRRC span))  = span
redundantConstraintsSpan _ = noSrcSpan

{-
-- Notes re TySynCtxt
-- We allow type synonyms that aren't types; e.g.  type List = []
--
-- If the RHS mentions tyvars that aren't in scope, we'll
-- quantify over them:
--      e.g.    type T = a->a
-- will become  type T = forall a. a->a
--
-- With gla-exts that's right, but for H98 we should complain.
-}


pprUserTypeCtxt :: UserTypeCtxt -> SDoc
pprUserTypeCtxt (FunSigCtxt n _)   = text "the type signature for" <+> quotes (ppr n)
pprUserTypeCtxt (InfSigCtxt n)     = text "the inferred type for" <+> quotes (ppr n)
pprUserTypeCtxt (ExprSigCtxt _)    = text "an expression type signature"
pprUserTypeCtxt KindSigCtxt        = text "a kind signature"
pprUserTypeCtxt (StandaloneKindSigCtxt n) = text "a standalone kind signature for" <+> quotes (ppr n)
pprUserTypeCtxt TypeAppCtxt       = text "a type argument"
pprUserTypeCtxt (ConArgCtxt c)    = text "the type of the constructor" <+> quotes (ppr c)
pprUserTypeCtxt (TySynCtxt c)     = text "the RHS of the type synonym" <+> quotes (ppr c)
pprUserTypeCtxt PatSigCtxt        = text "a pattern type signature"
pprUserTypeCtxt (ForSigCtxt n)    = text "the foreign declaration for" <+> quotes (ppr n)
pprUserTypeCtxt DefaultDeclCtxt   = text "a `default' declaration"
pprUserTypeCtxt (InstDeclCtxt False) = text "an instance declaration"
pprUserTypeCtxt (InstDeclCtxt True)  = text "a stand-alone deriving instance declaration"
pprUserTypeCtxt SpecInstCtxt      = text "a SPECIALISE instance pragma"
pprUserTypeCtxt GenSigCtxt        = text "a type expected by the context"
pprUserTypeCtxt (GhciCtxt {})     = text "a type in a GHCi command"
pprUserTypeCtxt (ClassSCCtxt c)   = text "the super-classes of class" <+> quotes (ppr c)
pprUserTypeCtxt SigmaCtxt         = text "the context of a polymorphic type"
pprUserTypeCtxt (DataTyCtxt tc)   = text "the context of the data type declaration for" <+> quotes (ppr tc)
pprUserTypeCtxt (PatSynCtxt n)    = text "the signature for pattern synonym" <+> quotes (ppr n)
pprUserTypeCtxt (DerivClauseCtxt) = text "a `deriving' clause"
pprUserTypeCtxt (TyVarBndrKindCtxt n) = text "the kind annotation on the type variable" <+> quotes (ppr n)
pprUserTypeCtxt (RuleBndrTypeCtxt n)  = text "the type signature for" <+> quotes (ppr n)
pprUserTypeCtxt (DataKindCtxt n)  = text "the kind annotation on the declaration for" <+> quotes (ppr n)
pprUserTypeCtxt (TySynKindCtxt n) = text "the kind annotation on the declaration for" <+> quotes (ppr n)
pprUserTypeCtxt (TyFamResKindCtxt n) = text "the result kind for" <+> quotes (ppr n)

isSigMaybe :: UserTypeCtxt -> Maybe Name
isSigMaybe (FunSigCtxt n _) = Just n
isSigMaybe (ConArgCtxt n)   = Just n
isSigMaybe (ForSigCtxt n)   = Just n
isSigMaybe (PatSynCtxt n)   = Just n
isSigMaybe _                = Nothing


{- *********************************************************************
*                                                                      *
          ErrCtxt
*                                                                      *
********************************************************************* -}
--------------------------------------------------------------------------------

-- type HsCtxtM = TidyEnv -> ZonkM (TidyEnv, HsCtxt)

-- | Additional context to include in an error message, e.g.
-- "In the type signature ...", "In the ambiguity check for ...", etc.
data ErrCtxt = MkErrCtxt

                 CodeSrcFlag
                 -- LandmarkUserSrcCode <=> this is a landmark context; do not
                 --                         discard it when trimming for display

                 HsCtxt
                 -- Monadic so that we have a chance
                 -- to deal with bound type variables just before error
                 -- message construction


data CodeSrcFlag = VanillaUserSrcCode
                 | LandmarkUserSrcCode
                 | ExpansionCodeCtxt

--------------------------------------------------------------------------------
-- Error message contexts

data UserSigType
  = UserLHsSigType !(LHsSigType GhcRn)
  | UserLHsType !(LHsType GhcRn)

instance Outputable UserSigType where
  ppr (UserLHsSigType ty) = ppr ty
  ppr (UserLHsType ty) = ppr ty


data FunAppCtxtFunArg
  = FunAppCtxtExpr !(HsExpr GhcRn) !(HsExpr GhcRn)
  | FunAppCtxtTy   !(LHsType GhcRn) !(LHsType GhcRn)

-- | Like 'TyConFlavour' but for instance declarations, with
-- the additional information of whether this we are dealing with
-- a default declaration.
data TyConInstFlavour
  = TyConInstFlavour
  { tyConInstFlavour :: !(TyConFlavour TyCon)
  , tyConInstIsDefault :: !Bool
  }

-- | The "context" of an error message, e.g. "In the expression <...>",
-- "In the pattern <...>", or "In the equations for closed type family <...>".
data HsCtxt
  -- | In an expression.
  = ExprCtxt !(HsExpr GhcRn)
  -- | In a user-written context.
  | ThetaCtxt !UserTypeCtxt !ThetaType
  -- | In a quantified constraint.
  | QuantifiedCtCtxt !PredType
  -- | When checking an inferred type.
  | InferredTypeCtxt !Name !TcType
  -- | In an inline pragma, or a fixity signature,
  -- or a type signature, or... (see 'Sig').
  | SigCtxt !(Sig GhcRn)
  -- | In a user-written type signature.
  | UserSigCtxt !UserTypeCtxt !UserSigType
  -- | In a record update.
  | RecordUpdCtxt !(NE.NonEmpty ConLike) ![Name] ![TyCoVar]
  -- | In a class method.
  | ClassOpCtxt !Id !Type
  -- | In the instance type signature of a class method.
  | MethSigCtxt !Name !TcType !TcType
  -- | In a pattern type signature.
  | PatSigErrCtxt !TcType !ExpType
  -- | In a pattern.
  | PatCtxt !(Pat GhcRn)
  -- | In a pattern synonym declaration.
  | PatSynDeclCtxt !Name
  -- | In a pattern matching context, e.g. a equation for a function binding,
  -- or a case alternative, ...
  | MatchCtxt !HsMatchContextRn
  -- | In a match in a pattern matching context,
  -- either for an expression or for an arrow command.
  | forall body. (Outputable body)
  => MatchInCtxt !(Match GhcRn body)
  -- | In a function application.
  | FunAppCtxt !FunAppCtxtFunArg !Int
  -- | In a function call.
  | FunTysCtxt !ExpectedFunTyCtxt !Type !Int !Int
  -- | In the result of a function call.
  | FunResCtxt !(HsExpr GhcTc) !Int !Type !Type !Int !Int
  -- | In the declaration of a type constructor.
  | TyConDeclCtxt !Name !(TyConFlavour TyCon)
  -- | In a type or data family instance (or default instance).
  | TyConInstCtxt !Name !TyConInstFlavour
  -- | In the declaration of a data constructor.
  | DataConDefCtxt !(NE.NonEmpty (LocatedN Name))
  -- | In the result type of a data constructor.
  | DataConResTyCtxt !(NE.NonEmpty (LocatedN Name))
  -- | In the equations for a closed type family.
  | ClosedFamEqnCtxt !TyCon
  -- | In the expansion of a type synonym.
  | TySynErrCtxt !TyCon
  -- | In a role annotation.
  | RoleAnnotErrCtxt !Name
  -- | In an arrow command.
  | CmdCtxt !(HsCmd GhcRn)
  -- | In an instance declaration.
  | InstDeclErrCtxt !(Either (LHsType GhcRn) PredType)
  -- | In a default declaration.
  | DefaultDeclErrCtxt { ddec_in_type_list :: !Bool }
  -- | In the body of a static form.
  | StaticFormCtxt !(LHsExpr GhcRn)
  -- | In a pattern binding.
  | forall p. OutputableBndrId p
  => PatMonoBindsCtxt !(LPat (GhcPass p)) !(GRHSs GhcRn (LHsExpr GhcRn))
  -- | In a foreign import/export declaration.
  | ForeignDeclCtxt !(ForeignDecl GhcRn)
  -- | In a record field.
  | FieldCtxt !FieldLabelString
  -- | In a type.
  | TypeCtxt !(LHsType GhcRn)
  -- | In a kind.
  | KindCtxt !(LHsKind GhcRn)
  -- | In an ambiguity check.
  | AmbiguityCheckCtxt !UserTypeCtxt !Bool

  -- | In a term-level use of a 'Name'.
  | TermLevelUseCtxt !Name !TermLevelUseCtxt

  -- | When checking the type of the @main@ function.
  | MainCtxt !Name
  -- | Warning emitted when inferring use of visible dependent quantification.
  | VDQWarningCtxt !TcTyCon

  -- | In a statement
  | forall body.
    ( Anno (StmtLR GhcRn GhcRn body) ~ SrcSpanAnnA
    , Outputable body
    ) => StmtErrCtxt !HsStmtContextRn !(LStmtLR GhcRn GhcRn body)

  -- | In patten of the do statement. (c.f. MonadFailErrors)
  | StmtErrCtxtPat (LPat GhcRn)

  -- | In an rebindable syntax expression.
  | SyntaxNameCtxt !(HsExpr GhcRn) !CtOrigin !TcType !SrcSpan
  -- | In a RULE.
  | RuleCtxt !FastString
  -- | In a subtype check.
  | SubTypeCtxt !TcType !TcType

  -- | In an export.
  | forall p. OutputableBndrId p
  => ExportCtxt (IE (GhcPass p))
  -- | In an export of a pattern synonym.
  | PatSynExportCtxt !PatSyn
  -- | In an export of a pattern synonym record field.
  | PatSynRecSelExportCtxt !PatSyn !Name

  -- | In an annotation.
  | forall p. OutputableBndrId p
  => AnnCtxt (AnnDecl (GhcPass p))

  -- | In a specialise pragma.
  | SpecPragmaCtxt !(Sig GhcRn)

  -- | In a deriving clause.
  | DerivInstCtxt !PredType
  -- | In a standalone deriving clause.
  | StandaloneDerivCtxt !(LHsSigWcType GhcRn)
  -- | When typechecking the body of a derived instance.
  | DerivBindCtxt !Id !Class ![Type]

  -- | In an untyped Template Haskell quote.
  | UntypedTHBracketCtxt !(HsQuote GhcPs)
  -- | In a typed Template Haskell quote.
  | forall p. OutputableBndrId p
  => TypedTHBracketCtxt !(LHsExpr (GhcPass p))
  -- | In an untyped Template Haskell splice or quasi-quote.
  | UntypedSpliceCtxt !(HsUntypedSplice GhcPs)
  -- | In a typed Template Haskell splice.
  | forall p. OutputableBndrId p
  => TypedSpliceCtxt !(Maybe SplicePointName) !(HsTypedSplice (GhcPass p))
  -- | In the result of a typed Template Haskell splice.
  | TypedSpliceResultCtxt !(LHsExpr GhcTc)
  -- | In an argument to the Template Haskell @reifyInstances@ function.
  | ReifyInstancesCtxt !TH.Name ![TH.Type]

  -- | While merging Backpack signatures.
  | MergeSignaturesCtxt !UnitState !ModuleName ![InstantiatedModule]
  -- | While checking that a module implements a Backpack signature.
  | CheckImplementsCtxt !UnitState !Module !InstantiatedModule
