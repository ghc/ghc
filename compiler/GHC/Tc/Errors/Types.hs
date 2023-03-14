{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module GHC.Tc.Errors.Types (
  -- * Main types
    TcRnMessage(..)
  , mkTcRnUnknownMessage
  , TcRnMessageDetailed(..)
  , TypeDataForbids(..)
  , ErrInfo(..)
  , FixedRuntimeRepProvenance(..)
  , pprFixedRuntimeRepProvenance
  , ShadowedNameProvenance(..)
  , RecordFieldPart(..)
  , IllegalNewtypeReason(..)
  , BadRecordUpdateReason(..)
  , InjectivityErrReason(..)
  , HasKinds(..)
  , hasKinds
  , SuggestUndecidableInstances(..)
  , suggestUndecidableInstances
  , SuggestUnliftedTypes(..)
  , DataSort(..), ppDataSort
  , AllowedDataResKind(..)
  , NotClosedReason(..)
  , SuggestPartialTypeSignatures(..)
  , suggestPartialTypeSignatures
  , DeriveInstanceErrReason(..)
  , UsingGeneralizedNewtypeDeriving(..)
  , usingGeneralizedNewtypeDeriving
  , DeriveAnyClassEnabled(..)
  , deriveAnyClassEnabled
  , DeriveInstanceBadConstructor(..)
  , HasWildcard(..)
  , hasWildcard
  , BadAnonWildcardContext(..)
  , SoleExtraConstraintWildcardAllowed(..)
  , DeriveGenericsErrReason(..)
  , HasAssociatedDataFamInsts(..)
  , hasAssociatedDataFamInsts
  , AssociatedTyLastVarInKind(..)
  , associatedTyLastVarInKind
  , AssociatedTyNotParamOverLastTyVar(..)
  , associatedTyNotParamOverLastTyVar
  , MissingSignature(..)
  , Exported(..)
  , HsDocContext(..)
  , FixedRuntimeRepErrorInfo(..)

  , ErrorItem(..), errorItemOrigin, errorItemEqRel, errorItemPred, errorItemCtLoc

  , SolverReport(..), SolverReportSupplementary(..)
  , SolverReportWithCtxt(..)
  , SolverReportErrCtxt(..)
  , getUserGivens, discardProvCtxtGivens
  , TcSolverReportMsg(..)
  , CannotUnifyVariableReason(..)
  , MismatchMsg(..)
  , MismatchEA(..)
  , mkPlainMismatchMsg, mkBasicMismatchMsg
  , WhenMatching(..)
  , ExpectedActualInfo(..)
  , TyVarInfo(..), SameOccInfo(..)
  , AmbiguityInfo(..)
  , CND_Extra(..)
  , FitsMbSuppressed(..)
  , ValidHoleFits(..), noValidHoleFits
  , HoleFitDispConfig(..)
  , RelevantBindings(..), pprRelevantBindings
  , PromotionErr(..), pprPECategory, peCategory
  , NotInScopeError(..), mkTcRnNotInScope
  , ImportError(..)
  , HoleError(..)
  , CoercibleMsg(..)
  , PotentialInstances(..)
  , UnsupportedCallConvention(..)
  , ExpectedBackends
  , ArgOrResult(..)
  , MatchArgsContext(..), MatchArgBadMatches(..)
  , ConversionFailReason(..)
  , UnrepresentableTypeDescr(..)
  , LookupTHInstNameErrReason(..)
  , SplicePhase(..)
  , THDeclDescriptor(..)
  , RunSpliceFailReason(..)
  , ThingBeingConverted(..)
  , IllegalDecls(..)
  , EmptyStatementGroupErrReason(..)
  , UnexpectedStatement(..)
  , DeclSort(..)
  , NonStandardGuards(..)
  , RuleLhsErrReason(..)
  , HsigShapeMismatchReason(..)
  , WrongThingSort(..)
  , StageCheckReason(..)
  , UninferrableTyVarCtx(..)
  , PatSynInvalidRhsReason(..)
  , BadFieldAnnotationReason(..)
  , SuperclassCycle(..)
  , SuperclassCycleDetail(..)
  , RoleValidationFailedReason(..)
  , DisabledClassExtension(..)
  , TyFamsDisabledReason(..)
  , HsTypeOrSigType(..)
  , HsTyVarBndrExistentialFlag(..)
  , TySynCycleTyCons
  ) where

import GHC.Prelude

import GHC.Hs
import {-# SOURCE #-} GHC.Tc.Types (TcIdSigInfo, TcTyThing)
import {-# SOURCE #-} GHC.Tc.Errors.Hole.FitTypes (HoleFit)
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Evidence (EvBindsVar)
import GHC.Tc.Types.Origin ( CtOrigin (ProvCtxtOrigin), SkolemInfoAnon (SigSkol)
                           , UserTypeCtxt (PatSynCtxt), TyVarBndrs, TypedThing
                           , FixedRuntimeRepOrigin(..), InstanceWhat )
import GHC.Tc.Types.Rank (Rank)
import GHC.Tc.Utils.TcType (IllegalForeignTypeReason, TcType, TcSigmaType, TcPredType)
import GHC.Types.Avail (AvailInfo)
import GHC.Types.Basic
import GHC.Types.Error
import GHC.Types.Hint (UntickedPromotedThing(..))
import GHC.Types.ForeignCall (CLabelString)
import GHC.Types.Id.Info ( RecSelParent(..) )
import GHC.Types.Name (Name, OccName, getSrcLoc, getSrcSpan)
import qualified GHC.Types.Name.Occurrence as OccName
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Types.TyThing (TyThing)
import GHC.Types.Var (Id, TyCoVar, TyVar, TcTyVar, CoVar)
import GHC.Types.Var.Env (TidyEnv)
import GHC.Types.Var.Set (TyVarSet, VarSet)
import GHC.Unit.Types (Module)
import GHC.Utils.Outputable
import GHC.Core.Class (Class, ClassMinimalDef)
import GHC.Core.Coercion (Coercion)
import GHC.Core.Coercion.Axiom (CoAxBranch)
import GHC.Core.ConLike (ConLike)
import GHC.Core.DataCon (DataCon, FieldLabel)
import GHC.Core.FamInstEnv (FamInst)
import GHC.Core.InstEnv (LookupInstanceErrReason, ClsInst)
import GHC.Core.PatSyn (PatSyn)
import GHC.Core.Predicate (EqRel, predTypeEqRel)
import GHC.Core.TyCon (TyCon, Role)
import GHC.Core.Type (Kind, Type, ThetaType, PredType)
import GHC.Driver.Backend (Backend)
import GHC.Unit.State (UnitState)
import GHC.Utils.Misc (capitalise, filterOut)
import qualified GHC.LanguageExtensions as LangExt
import GHC.Data.FastString (FastString)
import GHC.Exception.Type (SomeException)

import Language.Haskell.Syntax.Basic (FieldLabelString(..))

import qualified Data.List.NonEmpty as NE
import           Data.Typeable (Typeable)
import GHC.Unit.Module.Warnings (WarningCategory, WarningTxt)
import qualified Language.Haskell.TH.Syntax as TH

import GHC.Generics ( Generic )
import GHC.Types.Name.Env (NameEnv)
import GHC.Iface.Errors.Types

{-
Note [Migrating TcM Messages]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As part of #18516, we are slowly migrating the diagnostic messages emitted
and reported in the TcM from SDoc to TcRnMessage. Historically, GHC emitted
some diagnostics in 3 pieces, i.e. there were lots of error-reporting functions
that accepted 3 SDocs an input: one for the important part of the message,
one for the context and one for any supplementary information. Consider the following:

    • Couldn't match expected type ‘Int’ with actual type ‘Char’
    • In the expression: x4
      In a stmt of a 'do' block: return (x2, x4)
      In the expression:

Under the hood, the reporting functions in Tc.Utils.Monad were emitting "Couldn't match"
as the important part, "In the expression" as the context and "In a stmt..In the expression"
as the supplementary, with the context and supplementary usually smashed together so that
the final message would be composed only by two SDoc (which would then be bulleted like in
the example).

In order for us to smooth out the migration to the new diagnostic infrastructure, we
introduce the 'ErrInfo' and 'TcRnMessageDetailed' types, which serve exactly the purpose
of bridging the two worlds together without breaking the external API or the existing
format of messages reported by GHC.

Using 'ErrInfo' and 'TcRnMessageDetailed' also allows us to move away from the SDoc-ridden
diagnostic API inside Tc.Utils.Monad, enabling further refactorings.

In the future, once the conversion will be complete and we will successfully eradicate
any use of SDoc in the diagnostic reporting of GHC, we can surely revisit the usage and
existence of these two types, which for now remain a "necessary evil".

-}

-- The majority of TcRn messages come with extra context about the error,
-- and this newtype captures it. See Note [Migrating TcM Messages].
data ErrInfo = ErrInfo {
    errInfoContext :: !SDoc
    -- ^ Extra context associated to the error.
  , errInfoSupplementary :: !SDoc
    -- ^ Extra supplementary info associated to the error.
  }


-- | 'TcRnMessageDetailed' is an \"internal\" type (used only inside
-- 'GHC.Tc.Utils.Monad' that wraps a 'TcRnMessage' while also providing
-- any extra info needed to correctly pretty-print this diagnostic later on.
data TcRnMessageDetailed
  = TcRnMessageDetailed !ErrInfo
                        -- ^ Extra info associated with the message
                        !TcRnMessage
  deriving Generic

mkTcRnUnknownMessage :: (Diagnostic a, Typeable a, DiagnosticOpts a ~ NoDiagnosticOpts)
                     => a -> TcRnMessage
mkTcRnUnknownMessage diag = TcRnUnknownMessage (UnknownDiagnostic diag)

-- | An error which might arise during typechecking/renaming.
data TcRnMessage where
  {-| Simply wraps an unknown 'Diagnostic' message @a@. It can be used by plugins
      to provide custom diagnostic messages originated during typechecking/renaming.
  -}
  TcRnUnknownMessage :: UnknownDiagnostic -> TcRnMessage

  {-| Wrap an 'IfaceMessage' to a 'TcRnMessage' for when we attempt to load interface
      files during typechecking but encounter an error. -}

  TcRnInterfaceError :: !IfaceMessage -> TcRnMessage

  {-| TcRnMessageWithInfo is a constructor which is used when extra information is needed
      to be provided in order to qualify a diagnostic and where it was originated (and why).
      It carries an extra 'UnitState' which can be used to pretty-print some names
      and it wraps a 'TcRnMessageDetailed', which includes any extra context associated
      with this diagnostic.
  -}
  TcRnMessageWithInfo :: !UnitState
                      -- ^ The 'UnitState' will allow us to pretty-print
                      -- some diagnostics with more detail.
                      -> !TcRnMessageDetailed
                      -> TcRnMessage

  {-| TcRnWithHsDocContext annotates an error message with the context in which
      it originated.
  -}
  TcRnWithHsDocContext :: !HsDocContext
                       -> !TcRnMessage
                       -> TcRnMessage

  {-| TcRnSolverReport is the constructor used to report unsolved constraints
      after constraint solving, as well as other errors such as hole fit errors.

      See the documentation of the 'TcSolverReportMsg' datatype for an overview
      of the different errors.
  -}
  TcRnSolverReport :: SolverReportWithCtxt
                   -> DiagnosticReason
                   -> [GhcHint]
                   -> TcRnMessage
    -- TODO: split up TcRnSolverReport into several components,
    -- so that we can compute the reason and hints, as opposed
    -- to having to pass them here.

  {-| TcRnRedundantConstraints is a warning that is emitted when a binding
      has a user-written type signature which contains superfluous constraints.

      Example:

        f :: (Eq a, Ord a) => a -> a -> a
        f x y = (x < y) || x == y
          -- `Eq a` is superfluous: the `Ord a` constraint suffices.

      Test cases: T9939, T10632, T18036a, T20602, PluralS, T19296.
  -}
  TcRnRedundantConstraints :: [Id]
                           -> (SkolemInfoAnon, Bool)
                              -- ^ The contextual skolem info.
                              -- The boolean controls whether we
                              -- want to show it in the user message.
                              -- (Nice to keep track of the info in either case,
                              -- for other users of the GHC API.)
                           -> TcRnMessage

  {-| TcRnInaccessibleCode is a warning that is emitted when the RHS of a pattern
      match is inaccessible, because the constraint solver has detected a contradiction.

      Example:

        data B a where { MkTrue :: B True; MkFalse :: B False }

        foo :: B False -> Bool
        foo MkFalse = False
        foo MkTrue  = True -- Inaccessible: requires True ~ False

    Test cases: T7293, T7294, T15558, T17646, T18572, T18610, tcfail167.
  -}
  TcRnInaccessibleCode :: Implication          -- ^ The implication containing a contradiction.
                       -> SolverReportWithCtxt -- ^ The contradiction.
                       -> TcRnMessage

  {-| A type which was expected to have a fixed runtime representation
      does not have a fixed runtime representation.

      Example:

        data D (a :: TYPE r) = MkD a

      Test cases: T11724, T18534,
                  RepPolyPatSynArg, RepPolyPatSynUnliftedNewtype,
                  RepPolyPatSynRes, T20423
  -}
  TcRnTypeDoesNotHaveFixedRuntimeRep :: !Type
                                     -> !FixedRuntimeRepProvenance
                                     -> !ErrInfo -- Extra info accumulated in the TcM monad
                                     -> TcRnMessage

  {-| TcRnImplicitLift is a warning (controlled with -Wimplicit-lift) that occurs when
      a Template Haskell quote implicitly uses 'lift'.

     Example:
       warning1 :: Lift t => t -> Q Exp
       warning1 x = [| x |]

     Test cases: th/T17804
  -}
  TcRnImplicitLift :: Name -> !ErrInfo -> TcRnMessage

  {-| TcRnUnusedPatternBinds is a warning (controlled with -Wunused-pattern-binds)
      that occurs if a pattern binding binds no variables at all, unless it is a
      lone wild-card pattern, or a banged pattern.

     Example:
        Just _ = rhs3    -- Warning: unused pattern binding
        (_, _) = rhs4    -- Warning: unused pattern binding
        _  = rhs3        -- No warning: lone wild-card pattern
        !() = rhs4       -- No warning: banged pattern; behaves like seq

     Test cases: rename/{T13646,T17c,T17e,T7085}
  -}
  TcRnUnusedPatternBinds :: HsBind GhcRn -> TcRnMessage

  {-| TcRnUnusedQuantifiedTypeVar is a warning that occurs if there are unused
      quantified type variables.

      Examples:
        f :: forall a. Int -> Char

      Test cases: rename/should_compile/ExplicitForAllRules1
                  rename/should_compile/T5331
  -}
  TcRnUnusedQuantifiedTypeVar
    :: HsDocContext
    -> HsTyVarBndrExistentialFlag -- ^ tyVar binder.
    -> TcRnMessage

  {-| TcRnDodgyImports is a warning (controlled with -Wdodgy-imports) that occurs when
      an import of the form 'T(..)' or 'f(..)' does not actually import anything beside
      'T'/'f' itself.

     Test cases: rename/should_compile/T7167
  -}
  TcRnDodgyImports :: GlobalRdrElt -> TcRnMessage
  {-| TcRnDodgyExports is a warning (controlled by -Wdodgy-exports) that occurs when
      an export of the form 'T(..)' for a type constructor 'T' does not actually export anything
      beside 'T' itself.

     Example:
       module Foo (
           T(..)  -- Warning: T is a type synonym
         , A(..)  -- Warning: A is a type family
         , C(..)  -- Warning: C is a data family
         ) where

       type T = Int
       type family A :: * -> *
       data family C :: * -> *

     Test cases: warnings/should_compile/DodgyExports01
  -}
  TcRnDodgyExports :: GlobalRdrElt -> TcRnMessage
  {-| TcRnMissingImportList is a warning (controlled by -Wmissing-import-lists) that occurs when
      an import declaration does not explicitly list all the names brought into scope.

     Test cases: rename/should_compile/T4489
  -}
  TcRnMissingImportList :: IE GhcPs -> TcRnMessage
  {-| When a module marked trustworthy or unsafe (using -XTrustworthy or -XUnsafe) is compiled
      with a plugin, the TcRnUnsafeDueToPlugin warning (controlled by -Wunsafe) is used as the
      reason the module was inferred to be unsafe. This warning is not raised if the
      -fplugin-trustworthy flag is passed.

     Test cases: plugins/T19926
  -}
  TcRnUnsafeDueToPlugin :: TcRnMessage
  {-| TcRnModMissingRealSrcSpan is an error that occurs when compiling a module that lacks
      an associated 'RealSrcSpan'.

     Test cases: None
  -}
  TcRnModMissingRealSrcSpan :: Module -> TcRnMessage
  {-| TcRnIdNotExportedFromModuleSig is an error pertaining to backpack that occurs
      when an identifier required by a signature is not exported by the module
      or signature that is being used as a substitution for that signature.

      Example(s): None

     Test cases: backpack/should_fail/bkpfail36
  -}
  TcRnIdNotExportedFromModuleSig :: Name -> Module -> TcRnMessage
  {-| TcRnIdNotExportedFromLocalSig is an error pertaining to backpack that
      occurs when an identifier which is necessary for implementing a module
      signature is not exported from that signature.

      Example(s): None

     Test cases: backpack/should_fail/bkpfail30
                 backpack/should_fail/bkpfail31
                 backpack/should_fail/bkpfail34
  -}
  TcRnIdNotExportedFromLocalSig :: Name -> TcRnMessage

  {-| TcRnShadowedName is a warning (controlled by -Wname-shadowing) that occurs whenever
      an inner-scope value has the same name as an outer-scope value, i.e. the inner
      value shadows the outer one. This can catch typographical errors that turn into
      hard-to-find bugs. The warning is suppressed for names beginning with an underscore.

      Examples(s):
        f = ... let f = id in ... f ...  -- NOT OK, 'f' is shadowed
        f x = do { _ignore <- this; _ignore <- that; return (the other) } -- suppressed via underscore

     Test cases: typecheck/should_compile/T10971a
                 rename/should_compile/rn039
                 rename/should_compile/rn064
                 rename/should_compile/T1972
                 rename/should_fail/T2723
                 rename/should_compile/T3262
                 driver/werror
  -}
  TcRnShadowedName :: OccName -> ShadowedNameProvenance -> TcRnMessage

  {-| TcRnInvalidWarningCategory is an error that occurs when a warning is declared
      with a category name that is not the special category "deprecations", and
      either does not begin with the prefix "x-" indicating a user-defined
      category, or contains characters not valid in category names.  See Note
      [Warning categories] in GHC.Unit.Module.Warnings

      Examples(s):
        module M {-# WARNING in "invalid" "Oops" #-} where

        {-# WARNING in "x- spaces not allowed" foo "Oops" #-}

     Test cases: warnings/should_fail/WarningCategoryInvalid
  -}
  TcRnInvalidWarningCategory :: !WarningCategory -> TcRnMessage


  {-| TcRnDuplicateWarningDecls is an error that occurs whenever
      a warning is declared twice.

      Examples(s):
        None.

     Test cases:
        None.
  -}
  TcRnDuplicateWarningDecls :: !(LocatedN RdrName) -> !RdrName -> TcRnMessage

  {-| TcRnSimplifierTooManyIterations is an error that occurs whenever
      the constraint solver in the simplifier hits the iterations' limit.

      Examples(s):
        None.

     Test cases:
        None.
  -}
  TcRnSimplifierTooManyIterations :: Cts
                                  -> !IntWithInf
                                  -- ^ The limit.
                                  -> WantedConstraints
                                  -> TcRnMessage

  {-| TcRnIllegalPatSynDecl is an error that occurs whenever
      there is an illegal pattern synonym declaration.

      Examples(s):

      varWithLocalPatSyn x = case x of
          P -> ()
        where
          pattern P = ()   -- not valid, it can't be local, it must be defined at top-level.

     Test cases: patsyn/should_fail/local
  -}
  TcRnIllegalPatSynDecl :: !(LIdP GhcPs) -> TcRnMessage

  {-| TcRnLinearPatSyn is an error that occurs whenever a pattern
      synonym signature uses a field that is not unrestricted.

      Example(s): None

     Test cases: linear/should_fail/LinearPatSyn2
  -}
  TcRnLinearPatSyn :: !Type -> TcRnMessage

  {-| TcRnEmptyRecordUpdate is an error that occurs whenever
      a record is updated without specifying any field.

      Examples(s):

      $(deriveJSON defaultOptions{} ''Bad) -- not ok, no fields selected for update of defaultOptions

     Test cases: th/T12788
  -}
  TcRnEmptyRecordUpdate :: TcRnMessage

  {-| TcRnIllegalFieldPunning is an error that occurs whenever
      field punning is used without the 'NamedFieldPuns' extension enabled.

      Examples(s):

      data Foo = Foo { a :: Int }

      foo :: Foo -> Int
      foo Foo{a} = a  -- Not ok, punning used without extension.

     Test cases: parser/should_fail/RecordDotSyntaxFail12
  -}
  TcRnIllegalFieldPunning :: !(Located RdrName) -> TcRnMessage

  {-| TcRnIllegalWildcardsInRecord is an error that occurs whenever
      wildcards (..) are used in a record without the relevant
      extension being enabled.

      Examples(s):

      data Foo = Foo { a :: Int }

      foo :: Foo -> Int
      foo Foo{..} = a  -- Not ok, wildcards used without extension.

     Test cases: parser/should_fail/RecordWildCardsFail
  -}
  TcRnIllegalWildcardsInRecord :: !RecordFieldPart -> TcRnMessage

  {-| TcRnIllegalWildcardInType is an error that occurs
      when a wildcard appears in a type in a location in which
      wildcards aren't allowed.

      Examples:

        Type synonyms:

          type T = _

        Class declarations and instances:

          class C _
          instance C _

        Standalone kind signatures:

          type D :: _
          data D

      Test cases:
        ExtraConstraintsWildcardInTypeSplice2
        ExtraConstraintsWildcardInTypeSpliceUsed
        ExtraConstraintsWildcardNotLast
        ExtraConstraintsWildcardTwice
        NestedExtraConstraintsWildcard
        NestedNamedExtraConstraintsWildcard
        PartialClassMethodSignature
        PartialClassMethodSignature2
        T12039
        T13324_fail1
        UnnamedConstraintWildcard1
        UnnamedConstraintWildcard2
        WildcardInADT1
        WildcardInADT2
        WildcardInADT3
        WildcardInADTContext1
        WildcardInDefault
        WildcardInDefaultSignature
        WildcardInDeriving
        WildcardInForeignExport
        WildcardInForeignImport
        WildcardInGADT1
        WildcardInGADT2
        WildcardInInstanceHead
        WildcardInInstanceSig
        WildcardInNewtype
        WildcardInPatSynSig
        WildcardInStandaloneDeriving
        WildcardInTypeFamilyInstanceRHS
        WildcardInTypeSynonymRHS
        saks_fail003
        T15433a
  -}

  TcRnIllegalWildcardInType
    :: Maybe Name
        -- ^ the wildcard name, or 'Nothing' for an anonymous wildcard
    -> !BadAnonWildcardContext
    -> TcRnMessage

  {-| TcRnDuplicateFieldName is an error that occurs whenever
      there are duplicate field names in a single record.

      Examples(s):

        data R = MkR { x :: Int, x :: Bool }
        f r = r { x = 3, x = 4 }

     Test cases: T21959.
  -}
  TcRnDuplicateFieldName :: !RecordFieldPart -> NE.NonEmpty RdrName -> TcRnMessage

  {-| TcRnIllegalViewPattern is an error that occurs whenever
      the ViewPatterns syntax is used but the ViewPatterns language extension
      is not enabled.

      Examples(s):
      data Foo = Foo { a :: Int }

      foo :: Foo -> Int
      foo (a -> l) = l -- not OK, the 'ViewPattern' extension is not enabled.

     Test cases: parser/should_fail/ViewPatternsFail
  -}
  TcRnIllegalViewPattern :: !(Pat GhcPs) -> TcRnMessage

  {-| TcRnCharLiteralOutOfRange is an error that occurs whenever
      a character is out of range.

      Examples(s): None

     Test cases: None
  -}
  TcRnCharLiteralOutOfRange :: !Char -> TcRnMessage

  {-| TcRnNegativeNumTypeLiteral is an error that occurs whenever
      a type-level number literal is negative.

      type Neg = -1

     Test cases: th/T8412
                 typecheck/should_fail/T8306
  -}
  TcRnNegativeNumTypeLiteral :: HsType GhcPs -> TcRnMessage

  {-| TcRnIllegalWildcardsInConstructor is an error that occurs whenever
      the record wildcards '..' are used inside a constructor without labeled fields.

      Examples(s): None

     Test cases:
       rename/should_fail/T9815.hs
       rename/should_fail/T9815b.hs
       rename/should_fail/T9815ghci.hs
       rename/should_fail/T9815bghci.hs
  -}
  TcRnIllegalWildcardsInConstructor :: !Name -> TcRnMessage

  {-| TcRnIgnoringAnnotations is a warning that occurs when the source code
      contains annotation pragmas but the platform in use does not support an
      external interpreter such as GHCi and therefore the annotations are ignored.

      Example(s): None

     Test cases: None
  -}
  TcRnIgnoringAnnotations :: [LAnnDecl GhcRn] -> TcRnMessage

  {-| TcRnAnnotationInSafeHaskell is an error that occurs if annotation pragmas
      are used in conjunction with Safe Haskell.

      Example(s): None

     Test cases: annotations/should_fail/T10826
  -}
  TcRnAnnotationInSafeHaskell :: TcRnMessage

  {-| TcRnInvalidTypeApplication is an error that occurs when a visible type application
      is used with an expression that does not accept "specified" type arguments.

      Example(s):
      foo :: forall {a}. a -> a
      foo x = x
      bar :: ()
      bar = let x = foo @Int 42
            in ()

     Test cases: overloadedrecflds/should_fail/overloadedlabelsfail03
                 typecheck/should_fail/ExplicitSpecificity1
                 typecheck/should_fail/ExplicitSpecificity10
                 typecheck/should_fail/ExplicitSpecificity2
                 typecheck/should_fail/T17173
                 typecheck/should_fail/VtaFail
  -}
  TcRnInvalidTypeApplication :: Type -> LHsWcType GhcRn -> TcRnMessage

  {-| TcRnTagToEnumMissingValArg is an error that occurs when the 'tagToEnum#'
      function is not applied to a single value argument.

      Example(s):
      tagToEnum# 1 2

     Test cases: None
  -}
  TcRnTagToEnumMissingValArg :: TcRnMessage

  {-| TcRnTagToEnumUnspecifiedResTy is an error that occurs when the 'tagToEnum#'
      function is not given a concrete result type.

      Example(s):
      foo :: forall a. a
      foo = tagToEnum# 0#

     Test cases: typecheck/should_fail/tcfail164
  -}
  TcRnTagToEnumUnspecifiedResTy :: Type -> TcRnMessage

  {-| TcRnTagToEnumResTyNotAnEnum is an error that occurs when the 'tagToEnum#'
      function is given a result type that is not an enumeration type.

      Example(s):
      foo :: Int -- not an enumeration TyCon
      foo = tagToEnum# 0#

     Test cases: typecheck/should_fail/tcfail164
  -}
  TcRnTagToEnumResTyNotAnEnum :: Type -> TcRnMessage

  {-| TcRnTagToEnumResTyTypeData is an error that occurs when the 'tagToEnum#'
      function is given a result type that is headed by a @type data@ type, as
      the data constructors of a @type data@ do not exist at the term level.

      Example(s):
      type data Letter = A | B | C

      foo :: Letter
      foo = tagToEnum# 0#

     Test cases: type-data/should_fail/TDTagToEnum.hs
  -}
  TcRnTagToEnumResTyTypeData :: Type -> TcRnMessage

  {-| TcRnArrowIfThenElsePredDependsOnResultTy is an error that occurs when the
      predicate type of an ifThenElse expression in arrow notation depends on
      the type of the result.

      Example(s): None

     Test cases: None
  -}
  TcRnArrowIfThenElsePredDependsOnResultTy :: TcRnMessage

  {-| TcRnIllegalHsBootFileDecl is an error that occurs when an hs-boot file
      contains declarations that are not allowed, such as bindings.

      Example(s): None

     Test cases: None
  -}
  TcRnIllegalHsBootFileDecl :: TcRnMessage

  {-| TcRnRecursivePatternSynonym is an error that occurs when a pattern synonym
      is defined in terms of itself, either directly or indirectly.

      Example(s):
      pattern A = B
      pattern B = A

     Test cases: patsyn/should_fail/T16900
  -}
  TcRnRecursivePatternSynonym :: LHsBinds GhcRn -> TcRnMessage

  {-| TcRnPartialTypeSigTyVarMismatch is an error that occurs when a partial type signature
      attempts to unify two different types.

      Example(s):
      f :: a -> b -> _
      f x y = [x, y]

     Test cases: partial-sigs/should_fail/T14449
  -}
  TcRnPartialTypeSigTyVarMismatch
    :: Name -- ^ first type variable
    -> Name -- ^ second type variable
    -> Name -- ^ function name
    -> LHsSigWcType GhcRn -> TcRnMessage

  {-| TcRnPartialTypeSigBadQuantifier is an error that occurs when a type variable
      being quantified over in the partial type signature of a function gets unified
      with a type that is free in that function's context.

      Example(s):
      foo :: Num a => a -> a
      foo xxx = g xxx
        where
          g :: forall b. Num b => _ -> b
          g y = xxx + y

     Test cases: partial-sig/should_fail/T14479
  -}
  TcRnPartialTypeSigBadQuantifier
    :: Name   -- ^ user-written name of type variable being quantified
    -> Name   -- ^ function name
    -> Maybe Type   -- ^ type the variable unified with, if known
    -> LHsSigWcType GhcRn  -- ^ partial type signature
    -> TcRnMessage

  {-| TcRnMissingSignature is a warning that occurs when a top-level binding
      or a pattern synonym does not have a type signature.

      Controlled by the flags:
        -Wmissing-signatures
        -Wmissing-exported-signatures
        -Wmissing-pattern-synonym-signatures
        -Wmissing-exported-pattern-synonym-signatures
        -Wmissing-kind-signatures

      Test cases:
        T11077 (top-level bindings)
        T12484 (pattern synonyms)
        T19564 (kind signatures)
  -}
  TcRnMissingSignature :: MissingSignature
                       -> Exported
                       -> Bool -- ^ True: -Wmissing-signatures overrides -Wmissing-exported-signatures,
                               --     or -Wmissing-pattern-synonym-signatures overrides -Wmissing-exported-pattern-synonym-signatures
                       -> TcRnMessage

  {-| TcRnPolymorphicBinderMissingSig is a warning controlled by -Wmissing-local-signatures
      that occurs when a local polymorphic binding lacks a type signature.

      Example(s):
      id a = a

     Test cases: warnings/should_compile/T12574
  -}
  TcRnPolymorphicBinderMissingSig :: Name -> Type -> TcRnMessage

  {-| TcRnOverloadedSig is an error that occurs when a binding group conflicts
      with the monomorphism restriction.

      Example(s):
      data T a = T a
      mono = ... where
        x :: Applicative f => f a
        T x = ...

     Test cases: typecheck/should_compile/T11339
  -}
  TcRnOverloadedSig :: TcIdSigInfo -> TcRnMessage

  {-| TcRnTupleConstraintInst is an error that occurs whenever an instance
      for a tuple constraint is specified.

      Examples(s):
        class C m a
        class D m a
        f :: (forall a. Eq a => (C m a, D m a)) => m a
        f = undefined

      Test cases: quantified-constraints/T15334
  -}
  TcRnTupleConstraintInst :: !Class -> TcRnMessage

  {-| TcRnAbstractClassInst is an error that occurs whenever an instance
      of an abstract class is specified.

      Examples(s):
        -- A.hs-boot
        module A where
        class C a

        -- B.hs
        module B where
        import {-# SOURCE #-} A
        instance C Int where

        -- A.hs
        module A where
        import B
        class C a where
          f :: a

        -- Main.hs
        import A
        main = print (f :: Int)

      Test cases: typecheck/should_fail/T13068
  -}
  TcRnAbstractClassInst :: !Class -> TcRnMessage

  {-| TcRnNoClassInstHead is an error that occurs whenever an instance
      head is not headed by a class.

      Examples(s):
        instance c

      Test cases: typecheck/rename/T5513
                  typecheck/rename/T16385
  -}
  TcRnNoClassInstHead :: !Type -> TcRnMessage

  {-| TcRnUserTypeError is an error that occurs due to a user's custom type error,
      which can be triggered by adding a `TypeError` constraint in a type signature
      or typeclass instance.

      Examples(s):
        f :: TypeError (Text "This is a type error")
        f = undefined

      Test cases: typecheck/should_fail/CustomTypeErrors02
                  typecheck/should_fail/CustomTypeErrors03
  -}
  TcRnUserTypeError :: !Type -> TcRnMessage

  {-| TcRnConstraintInKind is an error that occurs whenever a constraint is specified
      in a kind.

      Examples(s):
        data Q :: Eq a => Type where {}

      Test cases: dependent/should_fail/T13895
                  polykinds/T16263
                  saks/should_fail/saks_fail004
                  typecheck/should_fail/T16059a
                  typecheck/should_fail/T18714
  -}
  TcRnConstraintInKind :: !Type -> TcRnMessage

  {-| TcRnUnboxedTupleTypeFuncArg is an error that occurs whenever an unboxed tuple
      or unboxed sum type is specified as a function argument, when the appropriate
      extension (`-XUnboxedTuples` or `-XUnboxedSums`) isn't enabled.

      Examples(s):
        -- T15073.hs
        import T15073a
        newtype Foo a = MkFoo a
          deriving P

        -- T15073a.hs
        class P a where
          p :: a -> (# a #)

      Test cases: deriving/should_fail/T15073.hs
                  deriving/should_fail/T15073a.hs
                  typecheck/should_fail/T16059d
  -}
  TcRnUnboxedTupleOrSumTypeFuncArg
    :: UnboxedTupleOrSum -- ^ whether this is an unboxed tuple or an unboxed sum
    -> !Type
    -> TcRnMessage

  {-| TcRnLinearFuncInKind is an error that occurs whenever a linear function is
      specified in a kind.

      Examples(s):
        data A :: * %1 -> *

      Test cases: linear/should_fail/LinearKind
                  linear/should_fail/LinearKind2
                  linear/should_fail/LinearKind3
  -}
  TcRnLinearFuncInKind :: !Type -> TcRnMessage

  {-| TcRnForAllEscapeError is an error that occurs whenever a quantified type's kind
      mentions quantified type variable.

      Examples(s):
        type T :: TYPE (BoxedRep l)
        data T = MkT

      Test cases: unlifted-datatypes/should_fail/UnlDataNullaryPoly
  -}
  TcRnForAllEscapeError :: !Type -> !Kind -> TcRnMessage

  {-| TcRnVDQInTermType is an error that occurs whenever a visible dependent quantification
      is specified in the type of a term.

      Examples(s):
        a = (undefined :: forall k -> k -> Type) @Int

      Test cases: dependent/should_fail/T15859
                  dependent/should_fail/T16326_Fail1
                  dependent/should_fail/T16326_Fail2
                  dependent/should_fail/T16326_Fail3
                  dependent/should_fail/T16326_Fail4
                  dependent/should_fail/T16326_Fail5
                  dependent/should_fail/T16326_Fail6
                  dependent/should_fail/T16326_Fail7
                  dependent/should_fail/T16326_Fail8
                  dependent/should_fail/T16326_Fail9
                  dependent/should_fail/T16326_Fail10
                  dependent/should_fail/T16326_Fail11
                  dependent/should_fail/T16326_Fail12
                  dependent/should_fail/T17687
                  dependent/should_fail/T18271
  -}
  TcRnVDQInTermType :: !(Maybe Type) -> TcRnMessage

  {-| TcRnBadQuantPredHead is an error that occurs whenever a quantified predicate
      lacks a class or type variable head.

      Examples(s):
        class (forall a. A t a => A t [a]) => B t where
          type A t a :: Constraint

      Test cases: quantified-constraints/T16474
  -}
  TcRnBadQuantPredHead :: !Type -> TcRnMessage

  {-| TcRnIllegalTupleConstraint is an error that occurs whenever an illegal tuple
      constraint is specified.

      Examples(s):
        g :: ((Show a, Num a), Eq a) => a -> a
        g = undefined

      Test cases: typecheck/should_fail/tcfail209a
  -}
  TcRnIllegalTupleConstraint :: !Type -> TcRnMessage

  {-| TcRnNonTypeVarArgInConstraint is an error that occurs whenever a non type-variable
      argument is specified in a constraint.

      Examples(s):
        data T
        instance Eq Int => Eq T

      Test cases: ghci/scripts/T13202
                  ghci/scripts/T13202a
                  polykinds/T12055a
                  typecheck/should_fail/T10351
                  typecheck/should_fail/T19187
                  typecheck/should_fail/T6022
                  typecheck/should_fail/T8883
  -}
  TcRnNonTypeVarArgInConstraint :: !Type -> TcRnMessage

  {-| TcRnIllegalImplicitParam is an error that occurs whenever an illegal implicit
      parameter is specified.

      Examples(s):
        type Bla = ?x::Int
        data T = T
        instance Bla => Eq T

      Test cases: polykinds/T11466
                  typecheck/should_fail/T8912
                  typecheck/should_fail/tcfail041
                  typecheck/should_fail/tcfail211
                  typecheck/should_fail/tcrun045
  -}
  TcRnIllegalImplicitParam :: !Type -> TcRnMessage

  {-| TcRnIllegalConstraintSynonymOfKind is an error that occurs whenever an illegal constraint
      synonym of kind is specified.

      Examples(s):
        type Showish = Show
        f :: (Showish a) => a -> a
        f = undefined

      Test cases: typecheck/should_fail/tcfail209
  -}
  TcRnIllegalConstraintSynonymOfKind :: !Type -> TcRnMessage

  {-| TcRnIllegalClassInst is an error that occurs whenever a class instance is specified
      for a non-class.

      Examples(s):
        type C1 a = (Show (a -> Bool))
        instance C1 Int where

      Test cases: polykinds/T13267
  -}
  TcRnIllegalClassInst :: !(TyConFlavour TyCon) -> TcRnMessage

  {-| TcRnOversaturatedVisibleKindArg is an error that occurs whenever an illegal oversaturated
      visible kind argument is specified.

      Examples(s):
        type family
          F2 :: forall (a :: Type). Type where
          F2 @a = Maybe a

      Test cases: typecheck/should_fail/T15793
                  typecheck/should_fail/T16255
  -}
  TcRnOversaturatedVisibleKindArg :: !Type -> TcRnMessage

  {-| TcRnBadAssociatedType is an error that occurs whenever a class doesn't have an
      associated type.

      Examples(s):
        $(do d <- instanceD (cxt []) (conT ''Eq `appT` conT ''Foo)
                    [tySynInstD $ tySynEqn Nothing (conT ''Rep `appT` conT ''Foo) (conT ''Maybe)]
             return [d])
        ======>
        instance Eq Foo where
          type Rep Foo = Maybe

      Test cases: th/T12387a
  -}
  TcRnBadAssociatedType :: {-Class-} !Name -> {-TyCon-} !Name -> TcRnMessage

  {-| TcRnForAllRankErr is an error that occurs whenever an illegal ranked type
      is specified.

      Examples(s):
        foo :: (a,b) -> (a~b => t) -> (a,b)
        foo p x = p

      Test cases:
        - ghci/should_run/T15806
        - indexed-types/should_fail/SimpleFail15
        - typecheck/should_fail/T11355
        - typecheck/should_fail/T12083a
        - typecheck/should_fail/T12083b
        - typecheck/should_fail/T16059c
        - typecheck/should_fail/T16059e
        - typecheck/should_fail/T17213
        - typecheck/should_fail/T18939_Fail
        - typecheck/should_fail/T2538
        - typecheck/should_fail/T5957
        - typecheck/should_fail/T7019
        - typecheck/should_fail/T7019a
        - typecheck/should_fail/T7809
        - typecheck/should_fail/T9196
        - typecheck/should_fail/tcfail127
        - typecheck/should_fail/tcfail184
        - typecheck/should_fail/tcfail196
        - typecheck/should_fail/tcfail197
  -}
  TcRnForAllRankErr :: !Rank -> !Type -> TcRnMessage

  {-| TcRnMonomorphicBindings is a warning (controlled by -Wmonomorphism-restriction)
      that arise when the monomorphism restriction applies to the given bindings.

      Examples(s):
        {-# OPTIONS_GHC -Wmonomorphism-restriction #-}

        bar = 10

        foo :: Int
        foo = bar

        main :: IO ()
        main = print foo

      The example above emits the warning (for 'bar'), because without monomorphism
      restriction the inferred type for 'bar' is 'bar :: Num p => p'. This warning tells us
      that /if/ we were to enable '-XMonomorphismRestriction' we would make 'bar'
      less polymorphic, as its type would become 'bar :: Int', so GHC warns us about that.

      Test cases: typecheck/should_compile/T13785
  -}
  TcRnMonomorphicBindings :: [Name] -> TcRnMessage

  {-| TcRnOrphanInstance is a warning (controlled by -Worphans) that arises when
      a typeclass instance or family instance is an \"orphan\", i.e. if it
      appears in a module in which neither the class/family nor the type being
      instanced are declared in the same module.

      Examples(s): None

      Test cases: warnings/should_compile/T9178
                  typecheck/should_compile/T4912
  -}
  TcRnOrphanInstance :: Either ClsInst FamInst -> TcRnMessage

  {-| TcRnFunDepConflict is an error that occurs when there are functional dependencies
      conflicts between instance declarations.

      Examples(s): None

      Test cases: typecheck/should_fail/T2307
                  typecheck/should_fail/tcfail096
                  typecheck/should_fail/tcfail202
  -}
  TcRnFunDepConflict :: !UnitState -> NE.NonEmpty ClsInst -> TcRnMessage

  {-| TcRnDupInstanceDecls is an error that occurs when there are duplicate instance
      declarations.

      Examples(s):
        class Foo a where
          foo :: a -> Int

        instance Foo Int where
          foo = id

        instance Foo Int where
          foo = const 42

      Test cases: cabal/T12733/T12733
                  typecheck/should_fail/tcfail035
                  typecheck/should_fail/tcfail023
                  backpack/should_fail/bkpfail18
                  typecheck/should_fail/TcNullaryTCFail
                  typecheck/should_fail/tcfail036
                  typecheck/should_fail/tcfail073
                  module/mod51
                  module/mod52
                  module/mod44
  -}
  TcRnDupInstanceDecls :: !UnitState -> NE.NonEmpty ClsInst -> TcRnMessage

  {-| TcRnConflictingFamInstDecls is an error that occurs when there are conflicting
      family instance declarations.

      Examples(s): None.

      Test cases: indexed-types/should_fail/ExplicitForAllFams4b
                  indexed-types/should_fail/NoGood
                  indexed-types/should_fail/Over
                  indexed-types/should_fail/OverDirectThisMod
                  indexed-types/should_fail/OverIndirectThisMod
                  indexed-types/should_fail/SimpleFail11a
                  indexed-types/should_fail/SimpleFail11b
                  indexed-types/should_fail/SimpleFail11c
                  indexed-types/should_fail/SimpleFail11d
                  indexed-types/should_fail/SimpleFail2a
                  indexed-types/should_fail/SimpleFail2b
                  indexed-types/should_fail/T13092/T13092
                  indexed-types/should_fail/T13092c/T13092c
                  indexed-types/should_fail/T14179
                  indexed-types/should_fail/T2334A
                  indexed-types/should_fail/T2677
                  indexed-types/should_fail/T3330b
                  indexed-types/should_fail/T4246
                  indexed-types/should_fail/T7102a
                  indexed-types/should_fail/T9371
                  polykinds/T7524
                  typecheck/should_fail/UnliftedNewtypesOverlap
  -}
  TcRnConflictingFamInstDecls :: NE.NonEmpty FamInst -> TcRnMessage

  TcRnFamInstNotInjective :: InjectivityErrReason -> TyCon -> NE.NonEmpty CoAxBranch -> TcRnMessage

  {-| TcRnBangOnUnliftedType is a warning (controlled by -Wredundant-strictness-flags) that
      occurs when a strictness annotation is applied to an unlifted type.

      Example(s):
      data T = MkT !Int# -- Strictness flag has no effect on unlifted types

     Test cases: typecheck/should_compile/T20187a
                 typecheck/should_compile/T20187b
  -}
  TcRnBangOnUnliftedType :: !Type -> TcRnMessage

  {-| TcRnLazyBangOnUnliftedType is a warning (controlled by -Wredundant-strictness-flags) that
      occurs when a lazy annotation is applied to an unlifted type.

      Example(s):
      data T = MkT ~Int# -- Lazy flag has no effect on unlifted types

     Test cases: typecheck/should_compile/T21951a
                 typecheck/should_compile/T21951b
  -}
  TcRnLazyBangOnUnliftedType :: !Type -> TcRnMessage

  {-| TcRnMultipleDefaultDeclarations is an error that occurs when a module has
      more than one default declaration.

      Example:
      default (Integer, Int)
      default (Double, Float) -- 2nd default declaration not allowed

     Text cases: module/mod58
  -}
  TcRnMultipleDefaultDeclarations :: [LDefaultDecl GhcRn] -> TcRnMessage

  {-| TcRnBadDefaultType is an error that occurs when a type used in a default
      declaration does not have an instance for any of the applicable classes.

      Example(s):
      data Foo
      default (Foo)

     Test cases: typecheck/should_fail/T11974b
  -}
  TcRnBadDefaultType :: Type -> [Class] -> TcRnMessage

  {-| TcRnPatSynBundledWithNonDataCon is an error that occurs when a module's
      export list bundles a pattern synonym with a type that is not a proper
      `data` or `newtype` construction.

      Example(s):
      module Foo (MyClass(.., P)) where
      pattern P = Nothing
      class MyClass a where
        foo :: a -> Int

     Test cases: patsyn/should_fail/export-class
  -}
  TcRnPatSynBundledWithNonDataCon :: TcRnMessage

  {-| TcRnPatSynBundledWithWrongType is an error that occurs when the export list
      of a module has a pattern synonym bundled with a type that does not match
      the type of the pattern synonym.

      Example(s):
      module Foo (R(P,x)) where
      data Q = Q Int
      data R = R
      pattern P{x} = Q x

     Text cases: patsyn/should_fail/export-ps-rec-sel
                 patsyn/should_fail/export-type-synonym
                 patsyn/should_fail/export-type
  -}
  TcRnPatSynBundledWithWrongType :: Type -> Type -> TcRnMessage

  {-| TcRnDupeModuleExport is a warning controlled by @-Wduplicate-exports@ that
      occurs when a module appears more than once in an export list.

      Example(s):
      module Foo (module Bar, module Bar)
      import Bar

     Text cases: None
  -}
  TcRnDupeModuleExport :: ModuleName -> TcRnMessage

  {-| TcRnExportedModNotImported is an error that occurs when an export list
      contains a module that is not imported.

      Example(s): None

     Text cases: module/mod135
                 module/mod8
                 rename/should_fail/rnfail028
                 backpack/should_fail/bkpfail48
  -}
  TcRnExportedModNotImported :: ModuleName -> TcRnMessage

  {-| TcRnNullExportedModule is a warning controlled by -Wdodgy-exports that occurs
      when an export list contains a module that has no exports.

      Example(s):
      module Foo (module Bar) where
      import Bar ()

     Test cases: None
  -}
  TcRnNullExportedModule :: ModuleName -> TcRnMessage

  {-| TcRnMissingExportList is a warning controlled by -Wmissing-export-lists that
      occurs when a module does not have an explicit export list.

      Example(s): None

     Test cases: typecheck/should_fail/MissingExportList03
  -}
  TcRnMissingExportList :: ModuleName -> TcRnMessage

  {-| TcRnExportHiddenComponents is an error that occurs when an export contains
      constructor or class methods that are not visible.

      Example(s): None

     Test cases: None
  -}
  TcRnExportHiddenComponents :: IE GhcPs -> TcRnMessage

  {-| TcRnDuplicateExport is a warning (controlled by -Wduplicate-exports) that occurs
      when an identifier appears in an export list more than once.

      Example(s): None

     Test cases: module/MultiExport
                 module/mod128
                 module/mod14
                 module/mod5
                 overloadedrecflds/should_fail/DuplicateExports
                 patsyn/should_compile/T11959
  -}
  TcRnDuplicateExport :: GlobalRdrElt -> IE GhcPs -> IE GhcPs -> TcRnMessage

  {-| TcRnExportedParentChildMismatch is an error that occurs when an export is
      bundled with a parent that it does not belong to

      Example(s):
      module Foo (T(a)) where
      data T
      a = True

     Test cases: module/T11970
                 module/T11970B
                 module/mod17
                 module/mod3
                 overloadedrecflds/should_fail/NoParent
  -}
  TcRnExportedParentChildMismatch :: Name -- ^ parent
                                  -> TyThing
                                  -> GlobalRdrElt -- ^ child
                                  -> [Name] -> TcRnMessage

  {-| TcRnConflictingExports is an error that occurs when different identifiers that
      have the same name are being exported by a module.

      Example(s):
      module Foo (Bar.f, module Baz) where
      import qualified Bar (f)
      import Baz (f)

     Test cases: module/mod131
                 module/mod142
                 module/mod143
                 module/mod144
                 module/mod145
                 module/mod146
                 module/mod150
                 module/mod155
                 overloadedrecflds/should_fail/T14953
                 overloadedrecflds/should_fail/overloadedrecfldsfail10
                 rename/should_fail/rnfail029
                 rename/should_fail/rnfail040
                 typecheck/should_fail/T16453E2
                 typecheck/should_fail/tcfail025
                 typecheck/should_fail/tcfail026
  -}
  TcRnConflictingExports
    :: OccName      -- ^ Occurrence name shared by both exports
    -> GlobalRdrElt -- ^ First export
    -> IE GhcPs     -- ^ Export decl of first export
    -> GlobalRdrElt -- ^ Second export
    -> IE GhcPs     -- ^ Export decl of second export
    -> TcRnMessage

  {-| TcRnDuplicateFieldExport is an error that occurs when a module exports
      multiple record fields with the same name, without enabling
      DuplicateRecordFields.

      Example:

      module M1 where
        data D1 = MkD1 { foo :: Int }
      module M2 where
        data D2 = MkD2 { foo :: Int }
      module M ( D1(..), D2(..) ) where
        import module M1
        import module M2

     Test case: overloadedrecflds/should_fail/overloadedrecfldsfail10
  -}
  TcRnDuplicateFieldExport
    :: (GlobalRdrElt, IE GhcPs)
    -> NE.NonEmpty (GlobalRdrElt, IE GhcPs)
    -> TcRnMessage

  {-| TcRnAmbiguousRecordUpdate is a warning, controlled by -Wambiguous-fields,
      which occurs when a user relies on the type-directed disambiguation
      mechanism to disambiguate a record update. This will not be supported by
      -XDuplicateRecordFields in future releases.

      Example(s):

        data Person  = MkPerson  { personId :: Int, name :: String }
        data Address = MkAddress { personId :: Int, address :: String }
        bad1 x = x { personId = 4 } :: Person -- ambiguous
        bad2 (x :: Person) = x { personId = 4 } -- ambiguous
        good x = (x :: Person) { personId = 4 } -- not ambiguous

     Test cases: overloadedrecflds/should_fail/overloadedrecfldsfail06
  -}
  TcRnAmbiguousRecordUpdate
    :: HsExpr GhcRn -- ^ Field update
    -> TyCon -- ^ Record type
    -> TcRnMessage

  {-| TcRnMissingFields is a warning controlled by -Wmissing-fields occurring
      when the intialisation of a record is missing one or more (lazy) fields.

      Example(s):
      data Rec = Rec { a :: Int, b :: String, c :: Bool }
      x = Rec { a = 1, b = "two" } -- missing field 'c'

     Test cases: deSugar/should_compile/T13870
                 deSugar/should_compile/ds041
                 patsyn/should_compile/T11283
                 rename/should_compile/T5334
                 rename/should_compile/T12229
                 rename/should_compile/T5892a
                 warnings/should_fail/WerrorFail2
  -}
  TcRnMissingFields :: ConLike -> [(FieldLabelString, TcType)] -> TcRnMessage

  {-| TcRnFieldUpdateInvalidType is an error occurring when an updated field's
      type mentions something that is outside the universally quantified variables
      of the data constructor, such as an existentially quantified type.

      Example(s):
      data X = forall a. MkX { f :: a }
      x = (MkX ()) { f = False }

      Test cases: patsyn/should_fail/records-exquant
                  typecheck/should_fail/T3323
  -}
  TcRnFieldUpdateInvalidType :: [(FieldLabelString,TcType)] -> TcRnMessage

  {- TcRnMissingStrictFields is an error occurring when a record field marked
     as strict is omitted when constructing said record.

     Example(s):
     data R = R { strictField :: !Bool, nonStrict :: Int }
     x = R { nonStrict = 1 }

    Test cases: typecheck/should_fail/T18869
                typecheck/should_fail/tcfail085
                typecheck/should_fail/tcfail112
  -}
  TcRnMissingStrictFields :: ConLike -> [(FieldLabelString, TcType)] -> TcRnMessage

  {-| TcRnAmbiguousFieldInUpdate is an error that occurs when a field in a
      record update clashes with another field or top-level function of the
      same name, and the user hasn't enabled -XDisambiguateRecordFields.

      Example:

        {-# LANGUAGE NoFieldSelectors #-}
        {-# LANGUAGE NoDisambiguateRecordFields #-}
        module M where

          data A = MkA { fld :: Int }

          fld :: Bool
          fld = False

          f r = r { fld = 3 }

  -}
  TcRnAmbiguousFieldInUpdate :: (GlobalRdrElt, GlobalRdrElt, [GlobalRdrElt])
                                -> TcRnMessage

  {-| TcRnBadRecordUpdate is an error when a regular (non-overloaded)
     record update cannot be pinned down to any one parent.

     The problem with the record update is stored in the 'BadRecordUpdateReason'
     field.

     Example(s):

       data R1 = R1 { x :: Int }
       data R2 = R2 { x :: Int }
       update r = r { x = 1 }
         -- ambiguous

       data R1 = R1 { x :: Int, y :: Int }
       data R2 = R2 { y :: Int, z :: Int }
       update r = r { x = 1, y = 2, z = 3 }
         -- no parent has all the fields

    Test cases: overloadedrecflds/should_fail/overloadedrecfldsfail01
                overloadedrecflds/should_fail/overloadedrecfldsfail01
                overloadedrecflds/should_fail/overloadedrecfldsfail14
  -}
  TcRnBadRecordUpdate :: [RdrName]
                         -- ^ the fields of the record update
                      -> BadRecordUpdateReason
                         -- ^ the reason this record update was rejected
                      -> TcRnMessage

  {- TcRnStaticFormNotClosed is an error pertaining to terms that are marked static
     using the -XStaticPointers extension but which are not closed terms.

     Example(s):
     f x = static x

    Test cases: rename/should_fail/RnStaticPointersFail01
                rename/should_fail/RnStaticPointersFail03
  -}
  TcRnStaticFormNotClosed :: Name -> NotClosedReason -> TcRnMessage
  {-| TcRnSpecialClassInst is an error that occurs when a user
      attempts to define an instance for a built-in typeclass such as
      'Coercible', 'Typeable', or 'KnownNat', outside of a signature file.

     Test cases: deriving/should_fail/T9687
                 deriving/should_fail/T14916
                 polykinds/T8132
                 typecheck/should_fail/TcCoercibleFail2
                 typecheck/should_fail/T12837
                 typecheck/should_fail/T14390

  -}
  TcRnSpecialClassInst :: !Class
                       -> !Bool -- ^ Whether the error is due to Safe Haskell being enabled
                       -> TcRnMessage

  {-| TcRnUselessTypeable is a warning (controlled by -Wderiving-typeable) that
      occurs when trying to derive an instance of the 'Typeable' class. Deriving
      'Typeable' is no longer necessary (hence the \"useless\") as all types
      automatically derive 'Typeable' in modern GHC versions.

      Example(s): None.

     Test cases: warnings/should_compile/DerivingTypeable
  -}
  TcRnUselessTypeable :: TcRnMessage

  {-| TcRnDerivingDefaults is a warning (controlled by -Wderiving-defaults) that
      occurs when both 'DeriveAnyClass' and 'GeneralizedNewtypeDeriving' are
      enabled, and therefore GHC defaults to 'DeriveAnyClass', which might not
      be what the user wants.

      Example(s): None.

     Test cases: typecheck/should_compile/T15839a
                 deriving/should_compile/T16179
  -}
  TcRnDerivingDefaults :: !Class -> TcRnMessage

  {-| TcRnNonUnaryTypeclassConstraint is an error that occurs when GHC
      encounters a non-unary constraint when trying to derive a typeclass.

      Example(s):
        class A
        deriving instance A
        data B deriving A  -- We cannot derive A, is not unary (i.e. 'class A a').

     Test cases: deriving/should_fail/T7959
                 deriving/should_fail/drvfail005
                 deriving/should_fail/drvfail009
                 deriving/should_fail/drvfail006
  -}
  TcRnNonUnaryTypeclassConstraint :: !(LHsSigType GhcRn) -> TcRnMessage

  {-| TcRnPartialTypeSignatures is a warning (controlled by -Wpartial-type-signatures)
      that occurs when a wildcard '_' is found in place of a type in a signature or a
      type class derivation

      Example(s):
        foo :: _ -> Int
        foo = ...

        deriving instance _ => Eq (Foo a)

     Test cases: dependent/should_compile/T11241
                 dependent/should_compile/T15076
                 dependent/should_compile/T14880-2
                 typecheck/should_compile/T17024
                 typecheck/should_compile/T10072
                 partial-sigs/should_fail/TidyClash2
                 partial-sigs/should_fail/Defaulting1MROff
                 partial-sigs/should_fail/WildcardsInPatternAndExprSig
                 partial-sigs/should_fail/T10615
                 partial-sigs/should_fail/T14584a
                 partial-sigs/should_fail/TidyClash
                 partial-sigs/should_fail/T11122
                 partial-sigs/should_fail/T14584
                 partial-sigs/should_fail/T10045
                 partial-sigs/should_fail/PartialTypeSignaturesDisabled
                 partial-sigs/should_fail/T10999
                 partial-sigs/should_fail/ExtraConstraintsWildcardInExpressionSignature
                 partial-sigs/should_fail/ExtraConstraintsWildcardInPatternSplice
                 partial-sigs/should_fail/WildcardInstantiations
                 partial-sigs/should_run/T15415
                 partial-sigs/should_compile/T10463
                 partial-sigs/should_compile/T15039a
                 partial-sigs/should_compile/T16728b
                 partial-sigs/should_compile/T15039c
                 partial-sigs/should_compile/T10438
                 partial-sigs/should_compile/SplicesUsed
                 partial-sigs/should_compile/T18008
                 partial-sigs/should_compile/ExprSigLocal
                 partial-sigs/should_compile/T11339a
                 partial-sigs/should_compile/T11670
                 partial-sigs/should_compile/WarningWildcardInstantiations
                 partial-sigs/should_compile/T16728
                 partial-sigs/should_compile/T12033
                 partial-sigs/should_compile/T15039b
                 partial-sigs/should_compile/T10403
                 partial-sigs/should_compile/T11192
                 partial-sigs/should_compile/T16728a
                 partial-sigs/should_compile/TypedSplice
                 partial-sigs/should_compile/T15039d
                 partial-sigs/should_compile/T11016
                 partial-sigs/should_compile/T13324_compile2
                 linear/should_fail/LinearPartialSig
                 polykinds/T14265
                 polykinds/T14172
  -}
  TcRnPartialTypeSignatures :: !SuggestPartialTypeSignatures -> !ThetaType -> TcRnMessage

  {-| TcRnCannotDeriveInstance is an error that occurs every time a typeclass instance
      can't be derived. The 'DeriveInstanceErrReason' will contain the specific reason
      this error arose.

      Example(s): None.

      Test cases: generics/T10604/T10604_no_PolyKinds
                  deriving/should_fail/drvfail009
                  deriving/should_fail/drvfail-functor2
                  deriving/should_fail/T10598_fail3
                  deriving/should_fail/deriving-via-fail2
                  deriving/should_fail/deriving-via-fail
                  deriving/should_fail/T16181
  -}
  TcRnCannotDeriveInstance :: !Class
                           -- ^ The typeclass we are trying to derive
                           -- an instance for
                           -> [Type]
                           -- ^ The typeclass arguments, if any.
                           -> !(Maybe (DerivStrategy GhcTc))
                           -- ^ The derivation strategy, if any.
                           -> !UsingGeneralizedNewtypeDeriving
                           -- ^ Is '-XGeneralizedNewtypeDeriving' enabled?
                           -> !DeriveInstanceErrReason
                           -- ^ The specific reason why we couldn't derive
                           -- an instance for the class.
                           -> TcRnMessage

  {-| TcRnLazyGADTPattern is an error that occurs when a user writes a nested
      GADT pattern match inside a lazy (~) pattern.

      Test case: gadt/lazypat
  -}
  TcRnLazyGADTPattern :: TcRnMessage

  {-| TcRnArrowProcGADTPattern is an error that occurs when a user writes a
      GADT pattern inside arrow proc notation.

      Test case: arrows/should_fail/arrowfail004.
  -}
  TcRnArrowProcGADTPattern :: TcRnMessage

  {-| TcRnForallIdentifier is a warning (controlled with -Wforall-identifier) that occurs
     when a definition uses 'forall' as an identifier.

     Example:
       forall x = ()
       g forall = ()

     Test cases: T20609 T20609a T20609b T20609c T20609d
  -}
  TcRnForallIdentifier :: RdrName -> TcRnMessage

  {-| TcRnCapturedTermName is a warning (controlled by -Wterm-variable-capture) that occurs
    when an implicitly quantified type variable's name is already used for a term.
    Example:
      a = 10
      f :: a -> a

    Test cases: T22513a T22513b T22513c T22513d T22513e T22513f T22513g T22513h T22513i
 -}
  TcRnCapturedTermName :: RdrName -> Either [GlobalRdrElt] Name -> TcRnMessage

  {-| TcRnTypeMultipleOccurenceOfBindVar is an error that occurs if a bound
      type variable's name is already in use.
    Example:
      f :: forall a. ...
      f (MkT @a ...) = ...

    Test cases: TyAppPat_ScopedTyVarConflict TyAppPat_NonlinearMultiPat TyAppPat_NonlinearMultiAppPat
  -}
  TcRnBindVarAlreadyInScope :: [LocatedN RdrName] -> TcRnMessage

  {-| TcRnBindMultipleVariables is an error that occurs in the case of
    multiple occurrences of a bound variable.
    Example:
      foo (MkFoo @(a,a) ...) = ...

    Test case: typecheck/should_fail/TyAppPat_NonlinearSinglePat
  -}
  TcRnBindMultipleVariables :: HsDocContext -> LocatedN RdrName -> TcRnMessage

  {-| TcRnTypeEqualityOutOfScope is a warning (controlled by -Wtype-equality-out-of-scope)
      that occurs when the type equality (a ~ b) is not in scope.

      Test case: warnings/should_compile/T18862b
  -}
  TcRnTypeEqualityOutOfScope :: TcRnMessage

  {-| TcRnTypeEqualityRequiresOperators is a warning (controlled by -Wtype-equality-requires-operators)
      that occurs when the type equality (a ~ b) is used without the TypeOperators extension.

      Example:
        {-# LANGUAGE NoTypeOperators #-}
        f :: (a ~ b) => a -> b

      Test case: T18862a
  -}
  TcRnTypeEqualityRequiresOperators :: TcRnMessage

  {-| TcRnIllegalTypeOperator is an error that occurs when a type operator
      is used without the TypeOperators extension.

      Example:
        {-# LANGUAGE NoTypeOperators #-}
        f :: Vec a n -> Vec a m -> Vec a (n + m)

      Test case: T12811
  -}
  TcRnIllegalTypeOperator :: !SDoc -> !RdrName -> TcRnMessage

  {-| TcRnIllegalTypeOperatorDecl is an error that occurs when a type or class
      operator is declared without the TypeOperators extension.

      See Note [Type and class operator definitions]

      Example:
        {-# LANGUAGE Haskell2010 #-}
        {-# LANGUAGE MultiParamTypeClasses #-}

        module T3265 where

        data a :+: b = Left a | Right b

        class a :*: b where {}


      Test cases: T3265, tcfail173
  -}
  TcRnIllegalTypeOperatorDecl :: !RdrName -> TcRnMessage


  {-| TcRnGADTMonoLocalBinds is a warning controlled by -Wgadt-mono-local-binds
      that occurs when pattern matching on a GADT when -XMonoLocalBinds is off.

      Example(s): None

      Test cases: T20485, T20485a
  -}
  TcRnGADTMonoLocalBinds :: TcRnMessage
  {-| The TcRnNotInScope constructor is used for various not-in-scope errors.
      See 'NotInScopeError' for more details. -}
  TcRnNotInScope :: NotInScopeError  -- ^ what the problem is
                 -> RdrName          -- ^ the name that is not in scope
                 -> [ImportError]    -- ^ import errors that are relevant
                 -> [GhcHint]        -- ^ hints, e.g. enable DataKinds to refer to a promoted data constructor
                 -> TcRnMessage
  TcRnTermNameInType :: RdrName -> [GhcHint] -> TcRnMessage

  {-| TcRnUntickedPromotedThing is a warning (controlled with -Wunticked-promoted-constructors)
      that is triggered by an unticked occurrence of a promoted data constructor.

      Examples:

        data A = MkA
        type family F (a :: A) where { F MkA = Bool }

        type B = [ Int, Bool ]

      Test cases: T9778, T19984.
  -}
  TcRnUntickedPromotedThing :: UntickedPromotedThing
                            -> TcRnMessage

  {-| TcRnIllegalBuiltinSyntax is an error that occurs when built-in syntax appears
      in an unexpected location, e.g. as a data constructor or in a fixity declaration.

      Examples:

        infixl 5 :

        data P = (,)

      Test cases: rnfail042, T14907b, T15124, T15233.
  -}
  TcRnIllegalBuiltinSyntax :: SDoc -- ^ what kind of thing this is (a binding, fixity declaration, ...)
                           -> RdrName
                           -> TcRnMessage
    -- TODO: remove the SDoc argument.

  {-| TcRnWarnDefaulting is a warning (controlled by -Wtype-defaults)
      that is triggered whenever a Wanted typeclass constraint
      is solving through the defaulting of a type variable.

      Example:

        one = show 1
        -- We get Wanteds Show a0, Num a0, and default a0 to Integer.

      Test cases:
        none (which are really specific to defaulting),
        but see e.g. tcfail204.
   -}
  TcRnWarnDefaulting :: [Ct] -- ^ Wanted constraints in which defaulting occurred
                     -> Maybe TyVar -- ^ The type variable being defaulted
                     -> Type -- ^ The default type
                     -> TcRnMessage

  {-| TcRnIncorrectNameSpace is an error that occurs when a 'Name'
      is used in the incorrect 'NameSpace', e.g. a type constructor
      or class used in a term, or a term variable used in a type.

      Example:

        f x = Int

      Test cases: T18740a, T20884.
  -}
  TcRnIncorrectNameSpace :: Name
                         -> Bool -- ^ whether the error is happening
                                 -- in a Template Haskell tick
                                 -- (so we should give a Template Haskell hint)
                         -> TcRnMessage

  {- TcRnForeignImportPrimExtNotSet is an error occurring when a foreign import
     is declared using the @prim@ calling convention without having turned on
     the -XGHCForeignImportPrim extension.

     Example(s):
     foreign import prim "foo" foo :: ByteArray# -> (# Int#, Int# #)

    Test cases: ffi/should_fail/T20116
  -}
  TcRnForeignImportPrimExtNotSet :: ForeignImport GhcRn -> TcRnMessage

  {- TcRnForeignImportPrimSafeAnn is an error declaring that the safe/unsafe
     annotation should not be used with @prim@ foreign imports.

     Example(s):
     foreign import prim unsafe "my_primop_cmm" :: ...

    Test cases: None
  -}
  TcRnForeignImportPrimSafeAnn :: ForeignImport GhcRn -> TcRnMessage

  {- TcRnForeignFunctionImportAsValue is an error explaining that foreign @value@
     imports cannot have function types.

     Example(s):
     foreign import capi "math.h value sqrt" f :: CInt -> CInt

    Test cases: ffi/should_fail/capi_value_function
  -}
  TcRnForeignFunctionImportAsValue :: ForeignImport GhcRn -> TcRnMessage

  {- TcRnFunPtrImportWithoutAmpersand is a warning controlled by @-Wdodgy-foreign-imports@
     that informs the user of a possible missing @&@ in the declaration of a
     foreign import with a 'FunPtr' return type.

     Example(s):
     foreign import ccall "f" f :: FunPtr (Int -> IO ())

    Test cases: ffi/should_compile/T1357
  -}
  TcRnFunPtrImportWithoutAmpersand :: ForeignImport GhcRn -> TcRnMessage

  {- TcRnIllegalForeignDeclBackend is an error occurring when a foreign import declaration
     is not compatible with the code generation backend being used.

     Example(s): None

    Test cases: None
  -}
  TcRnIllegalForeignDeclBackend
    :: Either (ForeignExport GhcRn) (ForeignImport GhcRn)
    -> Backend
    -> ExpectedBackends
    -> TcRnMessage

  {- TcRnUnsupportedCallConv informs the user that the calling convention specified
     for a foreign export declaration is not compatible with the target platform.
     It is a warning controlled by @-Wunsupported-calling-conventions@ in the case of
     @stdcall@ but is otherwise considered an error.

     Example(s): None

    Test cases: None
  -}
  TcRnUnsupportedCallConv :: Either (ForeignExport GhcRn) (ForeignImport GhcRn)
                          -> UnsupportedCallConvention
                          -> TcRnMessage

  {- TcRnIllegalForeignType is an error for when a type appears in a foreign
     function signature that is not compatible with the FFI.

     Example(s): None

    Test cases: ffi/should_fail/T3066
                ffi/should_fail/ccfail004
                ffi/should_fail/T10461
                ffi/should_fail/T7506
                ffi/should_fail/T5664
                safeHaskell/ghci/p6
                safeHaskell/safeLanguage/SafeLang08
                ffi/should_fail/T16702
                linear/should_fail/LinearFFI
                ffi/should_fail/T7243
  -}
  TcRnIllegalForeignType :: !(Maybe ArgOrResult) -> !IllegalForeignTypeReason -> TcRnMessage

  {- TcRnInvalidCIdentifier indicates a C identifier that is not valid.

     Example(s):
     foreign import prim safe "not valid" cmm_test2 :: Int# -> Int#

    Test cases: th/T10638
  -}
  TcRnInvalidCIdentifier :: !CLabelString -> TcRnMessage

  {- TcRnExpectedValueId is an error occurring when something that is not a
      value identifier is used where one is expected.

     Example(s): none

    Test cases: none
  -}
  TcRnExpectedValueId :: !TcTyThing -> TcRnMessage

  {- TcRnRecSelectorEscapedTyVar is an error indicating that a record field selector
     containing an existential type variable is used as a function rather than in
     a pattern match.

     Example(s):
     data Rec = forall a. Rec { field :: a }
     field (Rec True)

    Test cases: patsyn/should_fail/records-exquant
                typecheck/should_fail/T3176
  -}
  TcRnRecSelectorEscapedTyVar :: !OccName -> TcRnMessage

  {- TcRnPatSynNotBidirectional is an error for when a non-bidirectional pattern
     synonym is used as a constructor.

     Example(s):
     pattern Five :: Int
     pattern Five <- 5
     five = Five

    Test cases: patsyn/should_fail/records-no-uni-update
                patsyn/should_fail/records-no-uni-update2
  -}
  TcRnPatSynNotBidirectional :: !Name -> TcRnMessage

  {- TcRnSplicePolymorphicLocalVar is the error that occurs when the expression
     inside typed template haskell brackets is a polymorphic local variable.

     Example(s):
     x = \(y :: forall a. a -> a) -> [|| y ||]

    Test cases: quotes/T10384
  -}
  TcRnSplicePolymorphicLocalVar :: !Id -> TcRnMessage

  {- TcRnIllegalDerivingItem is an error for when something other than a type class
     appears in a deriving statement.

     Example(s):
     data X = X deriving Int

    Test cases: deriving/should_fail/T5922
  -}
  TcRnIllegalDerivingItem :: !(LHsSigType GhcRn) -> TcRnMessage

  {- TcRnUnexpectedAnnotation indicates the erroroneous use of an annotation such
     as strictness, laziness, or unpacking.

     Example(s):
     data T = T { t :: Maybe {-# UNPACK #-} Int }
     data C = C { f :: !IntMap Int }

    Test cases: parser/should_fail/unpack_inside_type
                typecheck/should_fail/T7210
  -}
  TcRnUnexpectedAnnotation :: !(HsType GhcRn) -> !HsSrcBang -> TcRnMessage

  {- TcRnIllegalRecordSyntax is an error indicating an illegal use of record syntax.

     Example(s):
     data T = T Int { field :: Int }

    Test cases: rename/should_fail/T7943
                rename/should_fail/T9077
  -}
  TcRnIllegalRecordSyntax :: Either (HsType GhcPs) (HsType GhcRn) -> TcRnMessage

  {- TcRnUnexpectedTypeSplice is an error for a typed template haskell splice
     appearing unexpectedly.

     Example(s): none

    Test cases: none
  -}
  TcRnUnexpectedTypeSplice :: !(HsType GhcRn) -> TcRnMessage

  {- TcRnInvalidVisibleKindArgument is an error for a kind application on a
     target type that cannot accept it.

     Example(s):
     bad :: Int @Type
     bad = 1
     type Foo :: forall a {b}. a -> b -> b
     type Foo x y = y
     type Bar = Foo @Bool @Int True 42

    Test cases: indexed-types/should_fail/T16356_Fail3
                typecheck/should_fail/ExplicitSpecificity7
                typecheck/should_fail/T12045b
                typecheck/should_fail/T12045c
                typecheck/should_fail/T15592a
                typecheck/should_fail/T15816
  -}
  TcRnInvalidVisibleKindArgument
    :: !(LHsType GhcRn) -- ^ The visible kind argument
    -> !Type -- ^ Target of the kind application
    -> TcRnMessage

  {- TcRnTooManyBinders is an error for a type constructor that is declared with
     more arguments then its kind specifies.

     Example(s):
     type T :: Type -> (Type -> Type) -> Type
     data T a (b :: Type -> Type) x1 (x2 :: Type -> Type)

    Test cases: saks/should_fail/saks_fail008
  -}
  TcRnTooManyBinders :: !Kind -> ![LHsTyVarBndr () GhcRn] -> TcRnMessage

  {- TcRnDifferentNamesForTyVar is an error that indicates different names being
     used for the same type variable.

     Example(s):
     data SameKind :: k -> k -> *
     data Q (a :: k1) (b :: k2) c = MkQ (SameKind a b)

    Test cases: polykinds/T11203
                polykinds/T11821a
                saks/should_fail/T20916
                typecheck/should_fail/T17566b
                typecheck/should_fail/T17566c
  -}
  TcRnDifferentNamesForTyVar :: !Name -> !Name -> TcRnMessage

  {- TcRnInvalidReturnKind is an error for a data declaration that has a kind signature
     with an invalid result kind.

     Example(s):
     data family Foo :: Constraint

    Test cases: typecheck/should_fail/T14048b
                typecheck/should_fail/UnliftedNewtypesConstraintFamily
                typecheck/should_fail/T12729
                typecheck/should_fail/T15883
                typecheck/should_fail/T16829a
                typecheck/should_fail/T16829b
                typecheck/should_fail/UnliftedNewtypesNotEnabled
                typecheck/should_fail/tcfail079
  -}
  TcRnInvalidReturnKind
    :: !DataSort -- ^ classification of thing being returned
    -> !AllowedDataResKind -- ^ allowed kind
    -> !Kind -- ^ the return kind
    -> !(Maybe SuggestUnliftedTypes) -- ^ suggested extension
    -> TcRnMessage

  {-| TcRnUnexpectedKindVar is an error that occurs when the user
      tries to use kind variables without -XPolyKinds.

      Example:
        f :: forall k a. Proxy (a :: k)

      Test cases: polykinds/BadKindVar
                  polykinds/T14710
                  saks/should_fail/T16722
  -}
  TcRnUnexpectedKindVar :: RdrName -> TcRnMessage

  {-| TcRnIllegalKind is used for a various illegal kinds errors including

      Example:
        type T :: forall k. Type -- without emabled -XPolyKinds

      Test cases: polykinds/T16762b
  -}
  TcRnIllegalKind
    :: HsTypeOrSigType GhcPs
            -- ^ The illegal kind
    -> Bool -- ^ Whether enabling -XPolyKinds should be suggested
    -> TcRnMessage

  {- TcRnClassKindNotConstraint is an error for a type class that has a kind that
     is not equivalent to Constraint.

     Example(s):
     type C :: Type -> Type
     class C a

    Test cases: saks/should_fail/T16826
  -}
  TcRnClassKindNotConstraint :: !Kind -> TcRnMessage

  {- TcRnUnpromotableThing is an error that occurs when the user attempts to
     use the promoted version of something which is not promotable.

     Example(s):
     data T :: T -> *
     data X a where
       MkX :: Show a => a -> X a
     foo :: Proxy ('MkX 'True)
     foo = Proxy

    Test cases: dependent/should_fail/PromotedClass
                dependent/should_fail/T14845_fail1
                dependent/should_fail/T14845_fail2
                dependent/should_fail/T15215
                dependent/should_fail/T13780c
                dependent/should_fail/T15245
                polykinds/T5716
                polykinds/T5716a
                polykinds/T6129
                polykinds/T7433
                patsyn/should_fail/T11265
                patsyn/should_fail/T9161-1
                patsyn/should_fail/T9161-2
                dependent/should_fail/SelfDep
                polykinds/PolyKinds06
                polykinds/PolyKinds07
                polykinds/T13625
                polykinds/T15116
                polykinds/T15116a
                saks/should_fail/T16727a
                saks/should_fail/T16727b
                rename/should_fail/T12686
  -}
  TcRnUnpromotableThing :: !Name -> !PromotionErr -> TcRnMessage

  {- TcRnMatchesHaveDiffNumArgs is an error occurring when something has matches
     that have different numbers of arguments

     Example(s):
     foo x = True
     foo x y = False

    Test cases: rename/should_fail/rnfail045
                typecheck/should_fail/T20768_fail
  -}
  TcRnMatchesHaveDiffNumArgs
    :: !(HsMatchContext GhcTc) -- ^ Pattern match specifics
    -> !MatchArgBadMatches
    -> TcRnMessage

  {-| TcRnUnexpectedPatSigType is an error occurring when there is
      a type signature in a pattern without -XScopedTypeVariables extension

      Examples:
        f (a :: Bool) = ...

      Test case: rename/should_fail/T11663
  -}
  TcRnUnexpectedPatSigType :: HsPatSigType GhcPs -> TcRnMessage

  {-| TcRnIllegalKindSignature is an error occuring when there is
      a kind signature without -XKindSignatures extension

      Examples:
        data Foo (a :: Nat) = ....

      Test case: parser/should_fail/readFail036
  -}
  TcRnIllegalKindSignature :: HsType GhcPs -> TcRnMessage

  {-| TcRnDataKindsError is an error occurring when there is
      an illegal type or kind, probably required -XDataKinds
      and is used without the enabled extension.

      Examples:

        type Foo = [Nat, Char]

        type Bar = [Int, String]

      Test cases: linear/should_fail/T18888
                  polykinds/T7151
                  th/TH_Promoted1Tuple
                  typecheck/should_fail/tcfail094
  -}
  TcRnDataKindsError :: TypeOrKind -> HsType GhcPs -> TcRnMessage

  {- TcRnCannotBindScopedTyVarInPatSig is an error stating that scoped type
     variables cannot be used in pattern bindings.

     Example(s):
     let (x :: a) = 5

     Test cases: typecheck/should_compile/tc141
  -}
  TcRnCannotBindScopedTyVarInPatSig :: !(NE.NonEmpty (Name, TcTyVar)) -> TcRnMessage

  {- TcRnCannotBindTyVarsInPatBind is an error for when type
     variables are introduced in a pattern binding

     Example(s):
     Just @a x = Just True

    Test cases: typecheck/should_fail/TyAppPat_PatternBinding
                typecheck/should_fail/TyAppPat_PatternBindingExistential
  -}
  TcRnCannotBindTyVarsInPatBind :: !(NE.NonEmpty (Name, TcTyVar)) -> TcRnMessage

  {- TcRnTooManyTyArgsInConPattern is an error occurring when a constructor pattern
     has more than the expected number of type arguments

     Example(s):
     f (Just @Int @Bool x) = x

    Test cases: typecheck/should_fail/TyAppPat_TooMany
                typecheck/should_fail/T20443b
  -}
  TcRnTooManyTyArgsInConPattern
    :: !ConLike
    -> !Int -- ^ Expected number of args
    -> !Int -- ^ Actual number of args
    -> TcRnMessage

  {- TcRnMultipleInlinePragmas is a warning signifying that multiple inline pragmas
     reference the same definition.

     Example(s):
     {-# INLINE foo #-}
     {-# INLINE foo #-}
     foo :: Bool -> Bool
     foo = id

    Test cases: none
  -}
  TcRnMultipleInlinePragmas
    :: !Id -- ^ Target of the pragmas
    -> !(LocatedA InlinePragma) -- ^ The first pragma
    -> !(NE.NonEmpty (LocatedA InlinePragma)) -- ^ Other pragmas
    -> TcRnMessage

  {- TcRnUnexpectedPragmas is a warning that occurs when unexpected pragmas appear
     in the source.

     Example(s):

    Test cases: none
  -}
  TcRnUnexpectedPragmas :: !Id -> !(NE.NonEmpty (LSig GhcRn)) -> TcRnMessage

  {- TcRnNonOverloadedSpecialisePragma is a warning for a specialise pragma being
     placed on a definition that is not overloaded.

     Example(s):
     {-# SPECIALISE foo :: Bool -> Bool #-}
     foo :: Bool -> Bool
     foo = id

    Test cases: simplCore/should_compile/T8537
                typecheck/should_compile/T10504
  -}
  TcRnNonOverloadedSpecialisePragma :: !(LIdP GhcRn) -> TcRnMessage

  {- TcRnSpecialiseNotVisible is a warning that occurs when the subject of a
     SPECIALISE pragma has a definition that is not visible from the current module.

     Example(s): none

    Test cases: none
  -}
  TcRnSpecialiseNotVisible :: !Name -> TcRnMessage

  {- TcRnPragmaWarning is a warning that can happen when usage of something
     is warned or deprecated by pragma.

    Test cases:
      DeprU
      T5281
      T5867
      rn050
      rn066 (here is a warning, not deprecation)
      T3303
  -}
  TcRnPragmaWarning :: {
    pragma_warning_occ :: OccName,
    pragma_warning_msg :: WarningTxt GhcRn,
    pragma_warning_import_mod :: ModuleName,
    pragma_warning_defined_mod :: ModuleName
  } -> TcRnMessage


  {-| TcRnIllegalHsigDefaultMethods is an error that occurs when a binding for
     a class default method is provided in a Backpack signature file.

    Test case:
      bkpfail40
  -}
  TcRnIllegalHsigDefaultMethods :: !Name -- ^ 'Name' of the class
                                -> NE.NonEmpty (LHsBind GhcRn) -- ^ default methods
                                -> TcRnMessage

  {-| TcRnHsigFixityMismatch is an error indicating that the fixity decl in a
    Backpack signature file differs from the one in the source file for the same
    operator.

    Test cases:
      bkpfail37, bkpfail38
  -}
  TcRnHsigFixityMismatch :: !TyThing -- ^ The operator whose fixity is defined
                         -> !Fixity -- ^ the fixity used in the source file
                         -> !Fixity -- ^ the fixity used in the signature
                         -> TcRnMessage

  {-| TcRnHsigShapeMismatch is a group of errors related to mismatches between
    backpack signatures.
  -}
  TcRnHsigShapeMismatch :: !HsigShapeMismatchReason
                         -> TcRnMessage

  {-| TcRnHsigMissingModuleExport is an error indicating that a module doesn't
    export a name exported by its signature.

    Test cases:
      bkpfail01, bkpfail05, bkpfail09, bkpfail16, bkpfail35, bkpcabal06
  -}
  TcRnHsigMissingModuleExport :: !OccName -- ^ The missing name
                              -> !UnitState -- ^ The module's unit state
                              -> !Module -- ^ The implementation module
                              -> TcRnMessage

  {-| TcRnBadGenericMethod
     This test ensures that if you provide a "more specific" type signatures
     for the default method, you must also provide a binding.

     Example:
     {-# LANGUAGE DefaultSignatures #-}

     class C a where
       meth :: a
       default meth :: Num a => a
       meth = 0

    Test case:
      testsuite/tests/typecheck/should_fail/MissingDefaultMethodBinding.hs
  -}
  TcRnBadGenericMethod :: !Name   -- ^ 'Name' of the class
                       -> !Name   -- ^ Problematic method
                       -> TcRnMessage

  {-| TcRnWarningMinimalDefIncomplete is a warning that one must
      specify which methods must be implemented by all instances.

     Example:
       class Cheater a where  -- WARNING LINE
       cheater :: a
       {-# MINIMAL #-} -- warning!

     Test case:
       testsuite/tests/warnings/minimal/WarnMinimal.hs:
  -}
  TcRnWarningMinimalDefIncomplete :: ClassMinimalDef -> TcRnMessage

  {-| TcRnDefaultMethodForPragmaLacksBinding is an error that occurs when
      a default method pragma is missing an accompanying binding.

    Test cases:
      testsuite/tests/typecheck/should_fail/T5084.hs
      testsuite/tests/typecheck/should_fail/T2354.hs
  -}
  TcRnDefaultMethodForPragmaLacksBinding
            :: Id             -- ^ method
            -> Sig GhcRn      -- ^ the pragma
            -> TcRnMessage
  {-| TcRnIgnoreSpecialisePragmaOnDefMethod is a warning that occurs when
      a specialise pragma is put on a default method.

    Test cases: none
  -}
  TcRnIgnoreSpecialisePragmaOnDefMethod
            :: !Name
            -> TcRnMessage
  {-| TcRnBadMethodErr is an error that happens when one attempts to provide a method
     in a class instance, when the class doesn't have a method by that name.

     Test case:
       testsuite/tests/th/T12387
  -}
  TcRnBadMethodErr
    :: { badMethodErrClassName  :: !Name
       , badMethodErrMethodName :: !Name
       } -> TcRnMessage
  {-| TcRnNoExplicitAssocTypeOrDefaultDeclaration is an error that occurs
      when a class instance does not provide an expected associated type
      or default declaration.

    Test cases:
      testsuite/tests/deriving/should_compile/T14094
      testsuite/tests/indexed-types/should_compile/Simple2
      testsuite/tests/typecheck/should_compile/tc254
  -}
  TcRnNoExplicitAssocTypeOrDefaultDeclaration
            :: Name
            -> TcRnMessage
  {-| TcRnIllegalNewtype is an error that occurs when a newtype:

      * Does not have exactly one field, or
      * is non-linear, or
      * is a GADT, or
      * has a context in its constructor's type, or
      * has existential type variables in its constructor's type, or
      * has strictness annotations.

    Test cases:
      testsuite/tests/gadt/T14719
      testsuite/tests/indexed-types/should_fail/T14033
      testsuite/tests/indexed-types/should_fail/T2334A
      testsuite/tests/linear/should_fail/LinearGADTNewtype
      testsuite/tests/parser/should_fail/readFail008
      testsuite/tests/polykinds/T11459
      testsuite/tests/typecheck/should_fail/T15523
      testsuite/tests/typecheck/should_fail/T15796
      testsuite/tests/typecheck/should_fail/T17955
      testsuite/tests/typecheck/should_fail/T18891a
      testsuite/tests/typecheck/should_fail/T21447
      testsuite/tests/typecheck/should_fail/tcfail156
  -}
  TcRnIllegalNewtype
            :: DataCon
            -> Bool -- ^ True if linear types enabled
            -> IllegalNewtypeReason
            -> TcRnMessage

  {-| TcRnIllegalTypeData is an error that occurs when a @type data@
      declaration occurs without the TypeOperators extension.

      See Note [Type data declarations]

     Test case:
       testsuite/tests/type-data/should_fail/TDNoPragma
  -}
  TcRnIllegalTypeData :: TcRnMessage

  {-| TcRnTypeDataForbids is an error that occurs when a @type data@
      declaration contains @data@ declaration features that are
      forbidden in a @type data@ declaration.

      See Note [Type data declarations]

     Test cases:
       testsuite/tests/type-data/should_fail/TDDeriving
       testsuite/tests/type-data/should_fail/TDRecordsGADT
       testsuite/tests/type-data/should_fail/TDRecordsH98
       testsuite/tests/type-data/should_fail/TDStrictnessGADT
       testsuite/tests/type-data/should_fail/TDStrictnessH98
  -}
  TcRnTypeDataForbids :: !TypeDataForbids -> TcRnMessage

  {-| TcRnTypedTHWithPolyType is an error that signifies the illegal use
      of a polytype in a typed template haskell expression.

      Example(s):
      bad :: (forall a. a -> a) -> ()
      bad = $$( [|| \_ -> () ||] )

     Test cases: th/T11452
  -}
  TcRnTypedTHWithPolyType :: !TcType -> TcRnMessage

  {-| TcRnSpliceThrewException is an error that occurrs when running a template
      haskell splice throws an exception.

      Example(s):

     Test cases: annotations/should_fail/annfail12
                 perf/compiler/MultiLayerModulesTH_Make
                 perf/compiler/MultiLayerModulesTH_OneShot
                 th/T10796b
                 th/T19470
                 th/T19709d
                 th/T5358
                 th/T5976
                 th/T7276a
                 th/T8987
                 th/TH_exn1
                 th/TH_exn2
                 th/TH_runIO
  -}
  TcRnSpliceThrewException
    :: !SplicePhase
    -> !SomeException
    -> !String -- ^ Result of showing the exception (cannot be done safely outside IO)
    -> !(LHsExpr GhcTc)
    -> !Bool -- True <=> Print the expression
    -> TcRnMessage

  {-| TcRnInvalidTopDecl is a template haskell error occurring when one of the 'Dec's passed to
      'addTopDecls' is not a function, value, annotation, or foreign import declaration.

      Example(s):

     Test cases:
  -}
  TcRnInvalidTopDecl :: !(HsDecl GhcPs) -> TcRnMessage

  {-| TcRnNonExactName is a template haskell error for when a declaration being
      added is bound to a name that is not fully known.

      Example(s):

     Test cases:
  -}
  TcRnNonExactName :: !RdrName -> TcRnMessage

  {-| TcRnAddInvalidCorePlugin is a template haskell error indicating that a
      core plugin being added has an invalid module due to being in the current package.

      Example(s):

     Test cases:
  -}
  TcRnAddInvalidCorePlugin
    :: !String -- ^ Module name
    -> TcRnMessage

  {-| TcRnAddDocToNonLocalDefn is a template haskell error for documentation being added to a
      definition which is not in the current module.

      Example(s):

     Test cases: showIface/should_fail/THPutDocExternal
  -}
  TcRnAddDocToNonLocalDefn :: !TH.DocLoc -> TcRnMessage

  {-| TcRnFailedToLookupThInstName is a template haskell error that occurrs when looking up an
      instance fails.

      Example(s):

     Test cases: showIface/should_fail/THPutDocNonExistent
  -}
  TcRnFailedToLookupThInstName :: !TH.Type -> !LookupTHInstNameErrReason -> TcRnMessage

  {-| TcRnCannotReifyInstance is a template haskell error for when an instance being reified
      via `reifyInstances` is not a class constraint or type family application.

      Example(s):

     Test cases:
  -}
  TcRnCannotReifyInstance :: !Type -> TcRnMessage

  {-| TcRnCannotReifyOutOfScopeThing is a template haskell error indicating
      that the given name is not in scope and therefore cannot be reified.

      Example(s):

     Test cases: th/T16976f
  -}
  TcRnCannotReifyOutOfScopeThing :: !TH.Name -> TcRnMessage

  {-| TcRnCannotReifyThingNotInTypeEnv is a template haskell error occurring
      when the given name is not in the type environment and therefore cannot be reified.

      Example(s):

     Test cases:
  -}
  TcRnCannotReifyThingNotInTypeEnv :: !Name -> TcRnMessage

  {-| TcRnNoRolesAssociatedWithName is a template haskell error for when the user
      tries to reify the roles of a given name but it is not something that has
      roles associated with it.

      Example(s):

     Test cases:
  -}
  TcRnNoRolesAssociatedWithThing :: !TcTyThing -> TcRnMessage

  {-| TcRnCannotRepresentThing is a template haskell error indicating that a
      type cannot be reified because it does not have a representation in template haskell.

      Example(s):

     Test cases:
  -}
  TcRnCannotRepresentType :: !UnrepresentableTypeDescr -> !Type -> TcRnMessage

  {-| TcRnRunSpliceFailure is an error indicating that a template haskell splice
      failed to be converted into a valid expression.

      Example(s):

     Test cases: th/T10828a
                 th/T10828b
                 th/T12478_4
                 th/T15270A
                 th/T15270B
                 th/T16895a
                 th/T16895b
                 th/T16895c
                 th/T16895d
                 th/T16895e
                 th/T18740d
                 th/T2597b
                 th/T2674
                 th/T3395
                 th/T7484
                 th/T7667a
                 th/TH_implicitParamsErr1
                 th/TH_implicitParamsErr2
                 th/TH_implicitParamsErr3
                 th/TH_invalid_add_top_decl
  -}
  TcRnRunSpliceFailure
    :: !(Maybe String) -- ^ Name of the function used to run the splice
    -> !RunSpliceFailReason
    -> TcRnMessage

  {-| TcRnUserErrReported is an error or warning thrown using 'qReport' from
      the 'Quasi' instance of 'TcM'.

      Example(s):

     Test cases:
  -}
  TcRnReportCustomQuasiError
    :: !Bool -- True => Error, False => Warning
    -> !String -- Error body
    -> TcRnMessage

  {- | TcRnUnsatisfiedMinimalDef is a warning that occurs when a class instance
       is missing methods that are required by the minimal definition.

       Example:
          class C a where
            foo :: a -> a
          instance C ()        -- | foo needs to be defined here

       Test cases:
         testsuite/tests/typecheck/prog001/typecheck.prog001
         testsuite/tests/typecheck/should_compile/tc126
         testsuite/tests/typecheck/should_compile/T7903
         testsuite/tests/typecheck/should_compile/tc116
         testsuite/tests/typecheck/should_compile/tc175
         testsuite/tests/typecheck/should_compile/HasKey
         testsuite/tests/typecheck/should_compile/tc125
         testsuite/tests/typecheck/should_compile/tc078
         testsuite/tests/typecheck/should_compile/tc161
         testsuite/tests/typecheck/should_fail/T5051
         testsuite/tests/typecheck/should_compile/T21583
         testsuite/tests/backpack/should_compile/bkp47
         testsuite/tests/backpack/should_fail/bkpfail25
         testsuite/tests/parser/should_compile/T2245
         testsuite/tests/parser/should_compile/read014
         testsuite/tests/indexed-types/should_compile/Class3
         testsuite/tests/indexed-types/should_compile/Simple2
         testsuite/tests/indexed-types/should_fail/T7862
         testsuite/tests/deriving/should_compile/deriving-1935
         testsuite/tests/deriving/should_compile/T9968a
         testsuite/tests/deriving/should_compile/drv003
         testsuite/tests/deriving/should_compile/T4966
         testsuite/tests/deriving/should_compile/T14094
         testsuite/tests/perf/compiler/T15304
         testsuite/tests/warnings/minimal/WarnMinimal
         testsuite/tests/simplCore/should_compile/simpl020
         testsuite/tests/deSugar/should_compile/T14546d
         testsuite/tests/ghci/scripts/T5820
         testsuite/tests/ghci/scripts/ghci019
  -}
  TcRnUnsatisfiedMinimalDef :: ClassMinimalDef -> TcRnMessage

  {- | 'TcRnMisplacedInstSig' is an error that happens when a method in
       a class instance is given a type signature, but the user has not
       enabled the @InstanceSigs@ extension.

       Test case:
       testsuite/tests/module/mod45
  -}
  TcRnMisplacedInstSig :: Name -> (LHsSigType GhcRn) -> TcRnMessage
  {- | 'TcRnBadBootFamInstDecl' is an error that is triggered by a
       type family instance being declared in an hs-boot file.

       Test case:
       testsuite/tests/indexed-types/should_fail/HsBootFam
  -}
  TcRnBadBootFamInstDecl :: {} -> TcRnMessage
  {- | 'TcRnIllegalFamilyInstance' is an error that occurs when an associated
       type or data family is given a top-level instance.

       Test case:
       testsuite/tests/indexed-types/should_fail/T3092
  -}
  TcRnIllegalFamilyInstance :: TyCon -> TcRnMessage
  {- | 'TcRnMissingClassAssoc' is an error that occurs when a class instance
       for a class with an associated type or data family is missing a corresponding
       family instance declaration.

       Test case:
       testsuite/tests/indexed-types/should_fail/SimpleFail7
  -}
  TcRnMissingClassAssoc :: TyCon -> TcRnMessage
  {- | 'TcRnNotOpenFamily' is an error that is triggered by attempting to give
       a top-level (open) type family instance for a closed type family.

       Test cases:
         testsuite/tests/indexed-types/should_fail/Overlap7
         testsuite/tests/indexed-types/should_fail/Overlap3
  -}
  TcRnNotOpenFamily :: TyCon -> TcRnMessage
  {-| TcRnNoRebindableSyntaxRecordDot is an error triggered by an overloaded record update
      without RebindableSyntax enabled.

      Example(s):

     Test cases: parser/should_fail/RecordDotSyntaxFail5
  -}
  TcRnNoRebindableSyntaxRecordDot :: TcRnMessage

  {-| TcRnNoFieldPunsRecordDot is an error triggered by the use of record field puns
      in an overloaded record update without enabling NamedFieldPuns.

      Example(s):
      print $ a{ foo.bar.baz.quux }

     Test cases: parser/should_fail/RecordDotSyntaxFail12
  -}
  TcRnNoFieldPunsRecordDot :: TcRnMessage

  {-| TcRnIllegalStaticExpression is an error thrown when user creates a static
      pointer via TemplateHaskell without enabling the StaticPointers extension.

      Example(s):

     Test cases: th/T14204
  -}
  TcRnIllegalStaticExpression :: HsExpr GhcPs -> TcRnMessage

  {-| TcRnIllegalStaticFormInSplice is an error when a user attempts to define
      a static pointer in a Template Haskell splice.

      Example(s):

     Test cases: th/TH_StaticPointers02
  -}
  TcRnIllegalStaticFormInSplice :: HsExpr GhcPs -> TcRnMessage

  {-| TcRnListComprehensionDuplicateBinding is an error triggered by duplicate
      let-bindings in a list comprehension.

      Example(s):
      [ () | let a = 13 | let a = 17 ]

     Test cases: typecheck/should_fail/tcfail092
  -}
  TcRnListComprehensionDuplicateBinding :: Name -> TcRnMessage

  {-| TcRnEmptyStmtsGroup is an error triggered by an empty list of statements
      in a statement block. For more information, see 'EmptyStatementGroupErrReason'

      Example(s):

        [() | then ()]

        do

        proc () -> do

     Test cases: rename/should_fail/RnEmptyStatementGroup1
  -}
  TcRnEmptyStmtsGroup:: EmptyStatementGroupErrReason -> TcRnMessage

  {-| TcRnLastStmtNotExpr is an error caused by the last statement
      in a statement block not being an expression.

      Example(s):

        do x <- pure ()

        do let x = 5

     Test cases: rename/should_fail/T6060
                 parser/should_fail/T3811g
                 parser/should_fail/readFail028
  -}
  TcRnLastStmtNotExpr
    :: HsStmtContext GhcRn
    -> UnexpectedStatement
    -> TcRnMessage

  {-| TcRnUnexpectedStatementInContext is an error when a statement appears
      in an unexpected context (e.g. an arrow statement appears in a list comprehension).

      Example(s):

     Test cases: parser/should_fail/readFail042
                 parser/should_fail/readFail038
                 parser/should_fail/readFail043
  -}
  TcRnUnexpectedStatementInContext
    :: HsStmtContext GhcRn
    -> UnexpectedStatement
    -> Maybe LangExt.Extension
    -> TcRnMessage

  {-| TcRnIllegalTupleSection is an error triggered by usage of a tuple section
      without enabling the TupleSections extension.

      Example(s):
        (5,)

     Test cases: rename/should_fail/rnfail056
  -}
  TcRnIllegalTupleSection :: TcRnMessage

  {-| TcRnIllegalImplicitParameterBindings is an error triggered by binding
      an implicit parameter in an mdo block.

      Example(s):
      mdo { let { ?x = 5 }; () }

     Test cases: rename/should_fail/RnImplicitBindInMdoNotation
  -}
  TcRnIllegalImplicitParameterBindings
    :: Either (HsLocalBindsLR GhcPs GhcPs) (HsLocalBindsLR GhcRn GhcPs)
    -> TcRnMessage

  {-| TcRnSectionWithoutParentheses is an error triggered by attempting to
      use an operator section without parentheses.

      Example(s):
      (`head` x, ())

     Test cases: rename/should_fail/T2490
                 rename/should_fail/T5657
  -}
  TcRnSectionWithoutParentheses :: HsExpr GhcPs -> TcRnMessage

  {- TcRnBindingOfExistingName is an error triggered by an attempt to rebind
     built-in syntax, punned list or tuple syntax, or a name quoted via Template Haskell.

     Examples:

       data []
       data (->)
       $(pure [ValD (VarP 'succ) (NormalB (ConE 'True)) []])

     Test cases: rename/should_fail/T14907b
                 rename/should_fail/T22839
                 rename/should_fail/rnfail042
                 th/T13968
  -}
  TcRnBindingOfExistingName :: RdrName -> TcRnMessage
  {-| TcRnMultipleFixityDecls is an error triggered by multiple
      fixity declarations for the same operator.

     Example(s):

       infixr 6 $$
       infixl 4 $$

     Test cases: rename/should_fail/RnMultipleFixityFail
  -}
  TcRnMultipleFixityDecls :: SrcSpan -> RdrName -> TcRnMessage

  {-| TcRnIllegalPatternSynonymDecl is an error thrown when a user
      defines a pattern synonyms without enabling the PatternSynonyms extension.

     Example:

       pattern O :: Int
       pattern O = 0

     Test cases: rename/should_fail/RnPatternSynonymFail
  -}
  TcRnIllegalPatternSynonymDecl :: TcRnMessage

  {-| TcRnIllegalClassBinding is an error triggered by a binding
      in a class or instance declaration of an illegal form.

     Examples:

        class ZeroOne a where
          zero :: a
          one :: a
        instance ZeroOne Int where
          (zero,one) = (0,1)

        class C a where
           pattern P = ()

     Test cases: module/mod48
                 patsyn/should_fail/T9705-1
                 patsyn/should_fail/T9705-2
                 typecheck/should_fail/tcfail021

  -}
  TcRnIllegalClassBinding :: DeclSort -> HsBindLR GhcPs GhcPs -> TcRnMessage

  {-| TcRnOrphanCompletePragma is an error triggered by a {-# COMPLETE #-}
      pragma which does not mention any data constructors or pattern synonyms
      defined in the current module.

     Test cases: patsyn/should_fail/T13349
  -}
  TcRnOrphanCompletePragma :: TcRnMessage

  {-| TcRnEmptyCase is an error thrown when a user uses
      a case expression with an empty list of alternatives without
      enabling the EmptyCase extension.

     Example(s):

       case () of

     Test cases: rename/should_fail/RnEmptyCaseFail
  -}
  TcRnEmptyCase :: HsMatchContext GhcRn -> TcRnMessage

  {-| TcRnNonStdGuards is a warning thrown when a user uses
      non-standard guards (e.g. patterns in guards) without
      enabling the PatternGuards extension.
      More realistically: the user has explicitly disabled PatternGuards,
      as it is enabled by default with `-XHaskell2010`.

     Example(s):

       f | 5 <- 2 + 3 = ...

     Test cases: rename/should_compile/rn049
  -}
  TcRnNonStdGuards :: NonStandardGuards -> TcRnMessage

  {-| TcRnDuplicateSigDecl is an error triggered by two or more
      signatures for one entity.

     Examples:

       f :: Int -> Bool
       f :: Int -> Bool
       f _ = True

       g x = x
       {-# INLINE g #-}
       {-# NOINLINE g #-}

       pattern P = ()
       {-# COMPLETE P #-}
       {-# COMPLETE P #-}

     Test cases: module/mod68
                 parser/should_fail/OpaqueParseFail4
                 patsyn/should_fail/T12165
                 rename/should_fail/rnfail048
                 rename/should_fail/T5589
                 rename/should_fail/T7338
                 rename/should_fail/T7338a
  -}
  TcRnDuplicateSigDecl :: NE.NonEmpty (LocatedN RdrName, Sig GhcPs) -> TcRnMessage

  {-| TcRnMisplacedSigDecl is an error triggered by the pragma application
      in the wrong context, like `MINIMAL` applied to a function or
      `SPECIALIZE` to an instance.

     Example:

       f x = x
       {-# MINIMAL f #-}

     Test cases: rename/should_fail/T18138
                 warnings/minimal/WarnMinimalFail1
  -}
  TcRnMisplacedSigDecl :: Sig GhcRn -> TcRnMessage

  {-| TcRnUnexpectedDefaultSig is an error thrown when a user uses
      default signatures without enabling the DefaultSignatures extension.

     Example:

       class C a where
         m :: a
         default m :: Num a => a
         m = 0

     Test cases: rename/should_fail/RnDefaultSigFail
  -}
  TcRnUnexpectedDefaultSig :: Sig GhcPs -> TcRnMessage

  {-| TcRnBindInBootFile is an error triggered by a binding in hs-boot file.

     Example:

       -- in an .hs-boot file:
       x = 3

     Test cases: rename/should_fail/T19781
  -}
  TcRnBindInBootFile :: TcRnMessage

  {-| TcRnDuplicateMinimalSig is an error triggered by two or more minimal
      signatures for one type class.

     Example:

       class C where
         f :: ()
         {-# MINIMAL f #-}
         {-# MINIMAL f #-}

     Test cases: rename/should_fail/RnMultipleMinimalPragmaFail
  -}
  TcRnDuplicateMinimalSig :: LSig GhcPs -> LSig GhcPs -> [LSig GhcPs] -> TcRnMessage

  {-| TcRnLoopySuperclassSolve is a warning, controlled by @-Wloopy-superclass-solve@,
      that is triggered when GHC solves a constraint in a possibly-loopy way,
      violating the class instance termination rules described in the section
      "Undecidable instances and loopy superclasses" of the user's guide.

      Example:

        class Foo f
        class Foo f => Bar f g
        instance Bar f f => Bar f (h k)

      Test cases: T20666, T20666{a,b}, T22891, T22912.
  -}
  TcRnLoopySuperclassSolve :: CtLoc    -- ^ Wanted 'CtLoc'
                           -> PredType -- ^ Wanted 'PredType'
                           -> TcRnMessage

  {-| TcRnIllegalInstanceHeadDecl is an error triggered by malformed head of
      type class instance

      Examples:

        instance 42

        instance !Show D

      Test cases: parser/should_fail/T3811c
                  rename/should_fail/T18240a
  -}
  TcRnIllegalInstanceHeadDecl :: LHsType GhcRn -> TcRnMessage

  {-| TcRnUnexpectedStandaloneDerivingDecl is an error thrown when a user uses
      standalone deriving without enabling the StandaloneDeriving extension.

      Example:

        deriving instance Eq Foo

      Test cases: rename/should_fail/RnUnexpectedStandaloneDeriving
  -}
  TcRnUnexpectedStandaloneDerivingDecl :: TcRnMessage

  {-| TcRnUnusedVariableInRuleDecl is an error triggered by forall'd variable in
      rewrite rule that does not appear on left-hand side

      Example:

        {-# RULES "rule" forall a. id = id #-}

      Test cases: rename/should_fail/ExplicitForAllRules2
  -}
  TcRnUnusedVariableInRuleDecl :: FastString -> Name -> TcRnMessage

  {-| TcRnUnexpectedStandaloneKindSig is an error thrown when a user uses standalone
      kind signature without enabling the StandaloneKindSignatures extension.

      Example:

        type D :: Type
        data D = D

      Test cases: saks/should_fail/saks_fail001
  -}
  TcRnUnexpectedStandaloneKindSig :: TcRnMessage

  {-| TcRnIllegalRuleLhs is an error triggered by malformed left-hand side
      of rewrite rule

      Examples:

        {-# RULES "test" forall x. f x = x #-}

        {-# RULES "test" forall x. case x of = x #-}

      Test cases: rename/should_fail/T15659
  -}
  TcRnIllegalRuleLhs
    :: RuleLhsErrReason
    -> FastString -- Rule name
    -> LHsExpr GhcRn -- Full expression
    -> HsExpr GhcRn -- Bad expression
    -> TcRnMessage

  {-| TcRnBadAssocRhs is an error triggered by out-of-scope type variables
      occurred in right-hand side of an associated type declaration

      Example:

        instance forall a. C Int where
          data instance D Int = MkD1 a

       cases: indexed-types/should_fail/T5515
                  polykinds/T9574
                  rename/should_fail/T18021
  -}
  TcRnBadAssocRhs :: [Name] -> TcRnMessage

  {-| TcRnDuplicateRoleAnnot is an error triggered by two or more role
      annotations for one type

      Example:

        data D a
        type role D phantom
        type role D phantom

      Test cases: roles/should_fail/Roles8
  -}
  TcRnDuplicateRoleAnnot :: NE.NonEmpty (LRoleAnnotDecl GhcPs) -> TcRnMessage

  {-| TcRnDuplicateKindSig is an error triggered by two or more standalone
      kind signatures for one type

      Example:

        type D :: Type
        type D :: Type
        data D

      Test cases: saks/should_fail/saks_fail002
  -}
  TcRnDuplicateKindSig :: NE.NonEmpty (LStandaloneKindSig GhcPs) -> TcRnMessage

  {-| TcRnIllegalDerivStrategy  is an error thrown when a user uses deriving
      strategy without enabling the DerivingStrategies extension or uses deriving
      via without enabling the DerivingVia extension.

      Examples:

        data T = T deriving stock Eq

        data T = T deriving via Eq T

      Test cases: deriving/should_fail/deriving-via-fail3
                  deriving/should_fail/T10598_fail4
  -}
  TcRnIllegalDerivStrategy :: DerivStrategy GhcPs -> TcRnMessage

  {-| TcRnIllegalMultipleDerivClauses is an error thrown when a user uses two or more
      deriving clauses without enabling the DerivingStrategies extension.

      Example:

        data T = T
          deriving Eq
          deriving Ord

      Test cases: deriving/should_fail/T10598_fail5
  -}
  TcRnIllegalMultipleDerivClauses :: TcRnMessage

  {-| TcRnNoDerivStratSpecified is a warning implied by -Wmissing-deriving-strategies
      and triggered by deriving clause without specified deriving strategy.

      Example:

        data T = T
          deriving Eq

      Test cases: rename/should_compile/T15798a
                  rename/should_compile/T15798b
                  rename/should_compile/T15798c
  -}
  TcRnNoDerivStratSpecified
    :: Bool -- True if DerivingStrategies is enabled
    -> TcRnMessage

  {-| TcRnStupidThetaInGadt is an error triggered by data contexts in GADT-style
      data declaration

      Example:

        data (Eq a) => D a where
          MkD :: D Int

      Test cases: rename/should_fail/RnStupidThetaInGadt
  -}
  TcRnStupidThetaInGadt :: HsDocContext -> TcRnMessage

  {-| TcRnBadImplicitSplice is an error thrown when a user uses top-level implicit
      TH-splice without enabling the TemplateHaskell extension.

      Example:

        pure [] -- on top-level

      Test cases: ghci/prog019/prog019
                  ghci/scripts/T1914
                  ghci/scripts/T6106
                  rename/should_fail/T4042
                  rename/should_fail/T12146
  -}
  TcRnBadImplicitSplice :: TcRnMessage

  {-| TcRnShadowedTyVarNameInFamResult is an error triggered by type variable in
      type family result that shadows type variable from left hand side

      Example:

        type family F a b c = b

      Test cases: ghci/scripts/T6018ghcirnfail
                  rename/should_fail/T6018rnfail
  -}
  TcRnShadowedTyVarNameInFamResult :: IdP GhcPs -> TcRnMessage

  {-| TcRnIncorrectTyVarOnRhsOfInjCond is an error caused by a situation where the
      left-hand side of an injectivity condition of a type family is not a variable
      referring to the type family result.
      See Note [Renaming injectivity annotation] for more details.

      Example:

        type family F a = r | a -> a

      Test cases: ghci/scripts/T6018ghcirnfail
                  rename/should_fail/T6018rnfail
  -}
  TcRnIncorrectTyVarOnLhsOfInjCond
    :: IdP GhcRn -- Expected
    -> LIdP GhcPs -- Actual
    -> TcRnMessage

  {-| TcRnUnknownTyVarsOnRhsOfInjCond is an error triggered by out-of-scope type
      variables on the right-hand side of a of an injectivity condition of a type family

      Example:

        type family F a = res | res -> b

      Test cases: ghci/scripts/T6018ghcirnfail
                  rename/should_fail/T6018rnfail
  -}
  TcRnUnknownTyVarsOnRhsOfInjCond :: [Name] -> TcRnMessage

  {-| TcRnLookupInstance groups several errors emitted when looking up class instances.

    Test cases:
      none
  -}
  TcRnLookupInstance
    :: !Class
    -> ![Type]
    -> !LookupInstanceErrReason
    -> TcRnMessage

  {-| TcRnBadlyStaged is an error that occurs when a TH binding is used in an
    invalid stage.

    Test cases:
      T17820d
  -}
  TcRnBadlyStaged
    :: !StageCheckReason -- ^ The binding being spliced.
    -> !Int -- ^ The binding stage.
    -> !Int -- ^ The stage at which the binding is used.
    -> TcRnMessage

  {-| TcRnStageRestriction is an error that occurs when a top level splice refers to
    a local name.

    Test cases:
      T17820, T21547, T5795, qq00[1-4], annfail0{3,4,6,9}
  -}
  TcRnStageRestriction
    :: !StageCheckReason -- ^ The binding being spliced.
    -> TcRnMessage

  {-| TcRnTyThingUsedWrong is an error that occurs when a thing is used where another
    thing was expected.

    Test cases:
      none
  -}
  TcRnTyThingUsedWrong
    :: !WrongThingSort -- ^ Expected thing.
    -> !TcTyThing -- ^ Thing used wrongly.
    -> !Name -- ^ Name of the thing used wrongly.
    -> TcRnMessage

  {-| TcRnCannotDefaultKindVar is an error that occurs when attempting to use
    unconstrained kind variables whose type isn't @Type@, without -XPolyKinds.

    Test cases:
      T11334b
  -}
  TcRnCannotDefaultKindVar
    :: !TyVar -- ^ The unconstrained variable.
    -> !Kind -- ^ Kind of the variable.
    -> TcRnMessage

  {-| TcRnUninferrableTyVar is an error that occurs when metavariables
    in a type could not be defaulted.

    Test cases:
      T17301, T17562, T17567, T17567StupidTheta, T15474, T21479
  -}
  TcRnUninferrableTyVar
    :: ![TyCoVar] -- ^ The variables that could not be defaulted.
    -> !UninferrableTyVarCtx -- ^ Description of the surrounding context.
    -> TcRnMessage

  {-| TcRnSkolemEscape is an error that occurs when type variables from an
    outer scope is used in a context where they should be locally scoped.

    Test cases:
      T15076, T15076b, T14880-2, T15825, T14880, T15807, T16946, T14350,
      T14040A, T15795, T15795a, T14552
  -}
  TcRnSkolemEscape
    :: ![TcTyVar] -- ^ The variables that would escape.
    -> !TcTyVar -- ^ The variable that is being quantified.
    -> !Type -- ^ The type in which they occur.
    -> TcRnMessage

  {-| TcRnPatSynEscapedCoercion is an error indicating that a coercion escaped from
    a pattern synonym into a type.
    See Note [Coercions that escape] in GHC.Tc.TyCl.PatSyn

    Test cases:
      T14507
  -}
  TcRnPatSynEscapedCoercion :: !Id -- ^ The pattern-bound variable
                            -> !(NE.NonEmpty CoVar) -- ^ The escaped coercions
                            -> TcRnMessage

  {-| TcRnPatSynExistentialInResult is an error indicating that the result type
    of a pattern synonym mentions an existential type variable.

    Test cases:
      PatSynExistential
  -}
  TcRnPatSynExistentialInResult :: !Name -- ^ The name of the pattern synonym
                                -> !TcSigmaType -- ^ The result type
                                -> ![TyVar] -- ^ The escaped existential variables
                                -> TcRnMessage

  {-| TcRnPatSynArityMismatch is an error indicating that the number of arguments in a
    pattern synonym's equation differs from the number of parameters in its
    signature.

    Test cases:
      PatSynArity
  -}
  TcRnPatSynArityMismatch :: !Name -- ^ The name of the pattern synonym
                          -> !Arity -- ^ The number of equation arguments
                          -> !Arity -- ^ The difference
                          -> TcRnMessage

  {-| TcRnPatSynInvalidRhs is an error group indicating that the pattern on the
    right hand side of a pattern synonym is invalid.

    Test cases:
      unidir, T14112
  -}
  TcRnPatSynInvalidRhs :: !Name -- ^ The name of the pattern synonym
                       -> !(LPat GhcRn) -- ^ The pattern
                       -> ![LIdP GhcRn] -- ^ The LHS args
                       -> !PatSynInvalidRhsReason -- ^ The number of equation arguments
                       -> TcRnMessage
  {-| TcRnCannotDefaultConcrete is an error occurring when a concrete
    type variable cannot be defaulted.

    Test cases:
      T23153
  -}
  TcRnCannotDefaultConcrete
    :: !FixedRuntimeRepOrigin
    -> TcRnMessage

  {-| TcRnMultiAssocTyFamDefaults is an error indicating that multiple default
    declarations were specified for an associated type family.

    Test cases:
      none
  -}
  TcRnMultiAssocTyFamDefaults :: !(IdP GhcRn) -- ^ Name of the associated type
                              -> TcRnMessage

  {-| TcRnTyFamDepsDisabled is an error indicating that a type family injectivity
    annotation was used without enabling the extension TypeFamilyDependencies.

    Test cases:
      T11381
  -}
  TcRnTyFamDepsDisabled :: TcRnMessage

  {-| TcRnAbstractClosedTyFamDecl is an error indicating that an abstract closed
    type family was declared in a regular source file, while it is only allowed
    in hs-boot files.

    Test cases:
      ClosedFam4
  -}
  TcRnAbstractClosedTyFamDecl :: TcRnMessage

  {-| TcRnPartialFieldSelector is a warning indicating that a record selector
    was not defined for all constructors of a data type.

    Test cases:
      DRFPartialFields, T7169
  -}
  TcRnPartialFieldSelector :: !FieldLabel -- ^ The selector
                           -> TcRnMessage

  {-| TcRnBadFieldAnnotation is an error/warning group indicating that a
    strictness/unpack related data type field annotation is invalid.
  -}
  TcRnBadFieldAnnotation :: !Int -- ^ The index of the field
                         -> !DataCon -- ^ The constructor in which the field is defined
                         -> !BadFieldAnnotationReason -- ^ The error specifics
                         -> TcRnMessage

  {-| TcRnSuperclassCycle is an error indicating that a class has a superclass
    cycle.

    Test cases:
      mod40, tcfail027, tcfail213, tcfail216, tcfail217, T9415, T9739
  -}
  TcRnSuperclassCycle :: !SuperclassCycle -- ^ The details of the cycle
                      -> TcRnMessage

  {-| TcRnDefaultSigMismatch is an error indicating that a default method
    signature doesn't match the regular method signature.

    Test cases:
      T7437, T12918a, T12918b, T12151
  -}
  TcRnDefaultSigMismatch :: !Id -- ^ The name of the method
                         -> !Type -- ^ The type of the default signature
                         -> TcRnMessage

  {-| TcRnTyFamsDisabled is an error indicating that a type family or instance
    was declared while the extension TypeFamilies was disabled.

    Test cases:
      TyFamsDisabled
  -}
  TcRnTyFamsDisabled :: !TyFamsDisabledReason -- ^ The name of the family or instance
                     -> TcRnMessage

  {-| TcRnTyFamResultDisabled is an error indicating that a result variable
    was used on a type family while the extension TypeFamilyDependencies was
    disabled.

    Test cases:
      T13571, T13571a
  -}
  TcRnTyFamResultDisabled :: !Name -- ^ The name of the type family
                          -> !(LHsTyVarBndr () GhcRn) -- ^ Name of the result variable
                          -> TcRnMessage

  {-| TcRnRoleValidationFailed is an error indicating that a variable was
    assigned an invalid role by the inference algorithm.
    This is only performed with -dcore-lint.
  -}
  TcRnRoleValidationFailed :: !Role -- ^ The validated role
                           -> !RoleValidationFailedReason -- ^ The failure reason
                           -> TcRnMessage

  {-| TcRnCommonFieldResultTypeMismatch is an error indicating that a sum type
    declares the same field name in multiple constructors, but the constructors'
    result types differ.

    Test cases:
      CommonFieldResultTypeMismatch
  -}
  TcRnCommonFieldResultTypeMismatch :: !DataCon -- ^ First constructor
                                    -> !DataCon -- ^ Second constructor
                                    -> !FieldLabelString -- ^ Field name
                                    -> TcRnMessage

  {-| TcRnCommonFieldTypeMismatch is an error indicating that a sum type
    declares the same field name in multiple constructors, but their types
    differ.

    Test cases:
      CommonFieldTypeMismatch
  -}
  TcRnCommonFieldTypeMismatch :: !DataCon -- ^ First constructor
                              -> !DataCon -- ^ Second constructor
                              -> !FieldLabelString -- ^ Field name
                              -> TcRnMessage

  {-| TcRnClassExtensionDisabled is an error indicating that a class
    was declared with an extension feature while the extension was disabled.
  -}
  TcRnClassExtensionDisabled :: !Class -- ^ The class
                             -> !DisabledClassExtension -- ^ The extension
                             -> TcRnMessage

  {-| TcRnAssocNoClassTyVar is an error indicating that no class parameters
    are used in an associated type family.

    Test cases:
      T2888, T9167, T12867
  -}
  TcRnAssocNoClassTyVar :: !Class -- ^ The class
                        -> !TyCon -- ^ The associated family
                        -> TcRnMessage

  {-| TcRnDataConParentTypeMismatch is an error indicating that a data
    constructor was declared with a type that doesn't match its type
    constructor (i.e. a GADT result type and its data name).

    Test cases:
      T7175, T13300, T14719, T18357, T18357b, gadt11, tcfail155, tcfail176
  -}
  TcRnDataConParentTypeMismatch :: !DataCon -- ^ The data constructor
                                -> !Type -- ^ The parent type
                                -> TcRnMessage

  {-| TcRnGADTsDisabled is an error indicating that a GADT was declared
    while the extension GADTs was disabled.

    Test cases:
      ghci057, T9293
  -}
  TcRnGADTsDisabled :: !Name -- ^ The name of the GADT
                    -> TcRnMessage

  {-| TcRnExistentialQuantificationDisabled is an error indicating that
    a data constructor was declared with existential features while the
    extension ExistentialQuantification was disabled.

    Test cases:
      ghci057, T9293, gadtSyntaxFail001, gadtSyntaxFail002, gadtSyntaxFail003,
      prog006, rnfail053, T12083a
  -}
  TcRnExistentialQuantificationDisabled :: !DataCon -- ^ The constructor
                                        -> TcRnMessage

  {-| TcRnGADTDataContext is an error indicating that a GADT was declared with a
    data type context.
    This error is emitted in the tc, but it is also caught in the renamer.
  -}
  TcRnGADTDataContext :: !Name -- ^ The data type name
                      -> TcRnMessage

  {-| TcRnMultipleConForNewtype is an error indicating that a newtype was
    declared with multiple constructors.
    This error is caught by the parser.
  -}
  TcRnMultipleConForNewtype :: !Name -- ^ The newtype name
                            -> !Int -- ^ The number of constructors
                            -> TcRnMessage

  {-| TcRnKindSignaturesDisabled is an error indicating that a kind signature
    was used in a data type declaration while the extension KindSignatures was
    disabled.

    Test cases:
      T20873c, readFail036
  -}
  TcRnKindSignaturesDisabled :: !(Either (HsType GhcPs) (Name, HsType GhcRn))
                                -- ^ The data type name
                             -> TcRnMessage

  {-| TcRnEmptyDataDeclsDisabled is an error indicating that a data type
    was declared with no constructors while the extension EmptyDataDecls was
    disabled.

    Test cases:
      readFail035
  -}
  TcRnEmptyDataDeclsDisabled :: !Name -- ^ The data type name
                             -> TcRnMessage

  {-| TcRnFamilyCategoryMismatch is an error indicating that a family instance
    was declared for a family of a different kind, i.e. data vs type family.

    Test cases:
      T9896, SimpleFail3a
  -}
  TcRnFamilyCategoryMismatch :: !TyCon -- ^ The family tycon
                             -> TcRnMessage

  {-| TcRnFamilyArityMismatch is an error indicating that a family instance
    was declared with a different number of arguments than the family.
    See Note [Oversaturated type family equations] in "GHC.Tc.Validity".

    Test cases:
      TyFamArity1, TyFamArity2, T11136, Overlap4, AssocTyDef05, AssocTyDef06,
      T14110
  -}
  TcRnFamilyArityMismatch :: !TyCon -- ^ The family tycon
                          -> !Arity -- ^ The right number of parameters
                          -> TcRnMessage

  {-| TcRnRoleMismatch is an error indicating that the role specified
    in an annotation differs from its inferred role.

    Test cases:
      T7253, Roles11
  -}
  TcRnRoleMismatch :: !Name -- ^ The type variable
                   -> !Role -- ^ The annotated role
                   -> !Role -- ^ The inferred role
                   -> TcRnMessage

  {-| TcRnRoleCountMismatch is an error indicating that the number of
    roles in an annotation doesn't match the number of type parameters.

    Test cases:
      Roles6
  -}
  TcRnRoleCountMismatch :: !Int -- ^ The number of type variables
                        -> !(LRoleAnnotDecl GhcRn) -- ^ The role annotation
                        -> TcRnMessage

  {-| TcRnIllegalRoleAnnotation is an error indicating that a role
    annotation was attached to a decl that doesn't allow it.

    Test cases:
      Roles5
  -}
  TcRnIllegalRoleAnnotation :: !(RoleAnnotDecl GhcRn) -- ^ The role annotation
                            -> TcRnMessage

  {-| TcRnRoleAnnotationsDisabled is an error indicating that a role
    annotation was declared while the extension RoleAnnotations was disabled.

    Test cases:
      Roles5, TH_Roles1
  -}
  TcRnRoleAnnotationsDisabled :: !TyCon -- ^ The annotated type
                              -> TcRnMessage

  {-| TcRnIncoherentRoles is an error indicating that a role
    annotation for a class parameter was declared as not nominal.

    Test cases:
      T8773
  -}
  TcRnIncoherentRoles :: !TyCon -- ^ The class tycon
                      -> TcRnMessage

  {-| TcRnIncoherentRoles is an error indicating that a type family equation
    used a different name than the family.

    Test cases:
      Overlap5, T15362, T16002, T20260, T11623
  -}
  TcRnTyFamNameMismatch :: !Name -- ^ The family name
                        -> !Name -- ^ The name used in the equation
                        -> TcRnMessage

  {-| TcRnPrecedenceParsingError is an error caused by attempting to
      use operators with the same precedence in one infix expression.

      Example:
        eq :: (a ~ b ~ c) :~: ()

      Test cases: module/mod61
                  parser/should_fail/readFail016
                  rename/should_fail/rnfail017
                  rename/should_fail/T9077
                  typecheck/should_fail/T18252a
  -}
  TcRnPrecedenceParsingError
    :: (OpName, Fixity) -- ^ first operator's name and fixity
    -> (OpName, Fixity) -- ^ second operator's name and fixity
    -> TcRnMessage

  {-| TcRnPrecedenceParsingError is an error caused by attempting to
      use an operator with higher precedence than the operand.

      Example:
        k = (-3 **)
          where
                (**) = const
                infixl 7 **

      Test cases: overloadedrecflds/should_fail/T13132_duplicaterecflds
                  parser/should_fail/readFail023
                  rename/should_fail/rnfail019
                  th/TH_unresolvedInfix2
  -}
  TcRnSectionPrecedenceError
    :: (OpName, Fixity) -- ^ first operator's name and fixity
    -> (OpName, Fixity) -- ^ argument operator
    -> HsExpr GhcPs -- ^ Section
    -> TcRnMessage

  {-| TcRnTypeSynonymCycle is an error indicating that a cycle between type
    synonyms has occurred.

    Test cases:
      mod27, ghc-e-fail2, bkpfail29
  -}
  TcRnTypeSynonymCycle :: !TySynCycleTyCons -- ^ The tycons involved in the cycle
                       -> TcRnMessage

  deriving Generic

-- | Things forbidden in @type data@ declarations.
-- See Note [Type data declarations]
data TypeDataForbids
  = TypeDataForbidsDatatypeContexts
  | TypeDataForbidsLabelledFields
  | TypeDataForbidsStrictnessAnnotations
  | TypeDataForbidsDerivingClauses
  deriving Generic

instance Outputable TypeDataForbids where
  ppr TypeDataForbidsDatatypeContexts      = text "Data type contexts"
  ppr TypeDataForbidsLabelledFields        = text "Labelled fields"
  ppr TypeDataForbidsStrictnessAnnotations = text "Strictness flags"
  ppr TypeDataForbidsDerivingClauses       = text "Deriving clauses"

data RunSpliceFailReason
  = ConversionFail !ThingBeingConverted !ConversionFailReason
  deriving Generic

-- | Identifies the TH splice attempting to be converted
data ThingBeingConverted
  = ConvDec !TH.Dec
  | ConvExp !TH.Exp
  | ConvPat !TH.Pat
  | ConvType !TH.Type

-- | The reason a TH splice could not be converted to a Haskell expression
data ConversionFailReason
  = IllegalOccName !OccName.NameSpace !String
  | SumAltArityExceeded !TH.SumAlt !TH.SumArity
  | IllegalSumAlt !TH.SumAlt
  | IllegalSumArity !TH.SumArity
  | MalformedType !TypeOrKind !TH.Type
  | IllegalLastStatement !HsDoFlavour !(LStmt GhcPs (LHsExpr GhcPs))
  | KindSigsOnlyAllowedOnGADTs
  | IllegalDeclaration !THDeclDescriptor !IllegalDecls
  | CannotMixGADTConsWith98Cons
  | EmptyStmtListInDoBlock
  | NonVarInInfixExpr
  | MultiWayIfWithoutAlts
  | CasesExprWithoutAlts
  | ImplicitParamsWithOtherBinds
  | InvalidCCallImpent !String -- ^ Source
  | RecGadtNoCons
  | GadtNoCons
  | InvalidTypeInstanceHeader !TH.Type
  | InvalidTyFamInstLHS !TH.Type
  | InvalidImplicitParamBinding
  | DefaultDataInstDecl ![LDataFamInstDecl GhcPs]
  | FunBindLacksEquations !TH.Name
  deriving Generic

data IllegalDecls
  = IllegalDecls    !(NE.NonEmpty (LHsDecl GhcPs))
  | IllegalFamDecls !(NE.NonEmpty (LFamilyDecl GhcPs))

-- | Label for a TH declaration
data THDeclDescriptor
  = InstanceDecl
  | WhereClause
  | LetBinding
  | LetExpression
  | ClssDecl

-- | Specifies which back ends can handle a requested foreign import or export
type ExpectedBackends = [Backend]

-- | Specifies which calling convention is unsupported on the current platform
data UnsupportedCallConvention
  = StdCallConvUnsupported
  | PrimCallConvUnsupported
  | JavaScriptCallConvUnsupported
  deriving Eq

-- | Whether the error pertains to a function argument or a result.
data ArgOrResult
  = Arg | Result

-- | Which parts of a record field are affected by a particular error or warning.
data RecordFieldPart
  = RecordFieldDecl !Name
  | RecordFieldConstructor !Name
  | RecordFieldPattern !Name
  | RecordFieldUpdate

-- | Why did we reject a record update?
data BadRecordUpdateReason
   -- | No constructor has all of the required fields.
   = NoConstructorHasAllFields
       { conflictingFields :: [FieldLabelString] }

   -- | There are several possible parents which have all of the required fields,
   -- and we weren't able to disambiguate in any way.
   | MultiplePossibleParents
       (RecSelParent, RecSelParent, [RecSelParent])
         -- ^ The possible parents (at least 2)

   -- | We used type-directed disambiguation, but this resulted in
   -- an invalid parent (the type-directed parent is not among the
   -- parents we computed from the field labels alone).
   | InvalidTyConParent TyCon (NE.NonEmpty RecSelParent)

  deriving Generic

-- | Where a shadowed name comes from
data ShadowedNameProvenance
  = ShadowedNameProvenanceLocal !SrcLoc
    -- ^ The shadowed name is local to the module
  | ShadowedNameProvenanceGlobal [GlobalRdrElt]
    -- ^ The shadowed name is global, typically imported from elsewhere.

-- | In what context did we require a type to have a fixed runtime representation?
--
-- Used by 'GHC.Tc.Utils.TcMType.checkTypeHasFixedRuntimeRep' for throwing
-- representation polymorphism errors when validity checking.
--
-- See Note [Representation polymorphism checking] in GHC.Tc.Utils.Concrete
data FixedRuntimeRepProvenance
  -- | Data constructor fields must have a fixed runtime representation.
  --
  -- Tests: T11734, T18534.
  = FixedRuntimeRepDataConField

  -- | Pattern synonym signature arguments must have a fixed runtime representation.
  --
  -- Test: RepPolyPatSynArg.
  | FixedRuntimeRepPatSynSigArg

  -- | Pattern synonym signature scrutinee must have a fixed runtime representation.
  --
  -- Test: RepPolyPatSynRes.
  | FixedRuntimeRepPatSynSigRes

pprFixedRuntimeRepProvenance :: FixedRuntimeRepProvenance -> SDoc
pprFixedRuntimeRepProvenance FixedRuntimeRepDataConField = text "data constructor field"
pprFixedRuntimeRepProvenance FixedRuntimeRepPatSynSigArg = text "pattern synonym argument"
pprFixedRuntimeRepProvenance FixedRuntimeRepPatSynSigRes = text "pattern synonym scrutinee"

-- | Why the particular illegal newtype error arose together with more
-- information, if any.
data IllegalNewtypeReason
  = DoesNotHaveSingleField !Int
  | IsNonLinear
  | IsGADT
  | HasConstructorContext
  | HasExistentialTyVar
  | HasStrictnessAnnotation
  deriving Generic

-- | Why the particular injectivity error arose together with more information,
-- if any.
data InjectivityErrReason
  = InjErrRhsBareTyVar [Type]
  | InjErrRhsCannotBeATypeFam
  | InjErrRhsOverlap
  | InjErrCannotInferFromRhs !TyVarSet !HasKinds !SuggestUndecidableInstances

data HasKinds
  = YesHasKinds
  | NoHasKinds
  deriving (Show, Eq)

hasKinds :: Bool -> HasKinds
hasKinds True  = YesHasKinds
hasKinds False = NoHasKinds

data SuggestUndecidableInstances
  = YesSuggestUndecidableInstaces
  | NoSuggestUndecidableInstaces
  deriving (Show, Eq)

suggestUndecidableInstances :: Bool -> SuggestUndecidableInstances
suggestUndecidableInstances True  = YesSuggestUndecidableInstaces
suggestUndecidableInstances False = NoSuggestUndecidableInstaces

data SuggestUnliftedTypes
  = SuggestUnliftedNewtypes
  | SuggestUnliftedDatatypes

-- | A description of whether something is a
--
-- * @data@ or @newtype@ ('DataDeclSort')
--
-- * @data instance@ or @newtype instance@ ('DataInstanceSort')
--
-- * @data family@ ('DataFamilySort')
--
-- At present, this data type is only consumed by 'checkDataKindSig'.
data DataSort
  = DataDeclSort     NewOrData
  | DataInstanceSort NewOrData
  | DataFamilySort

ppDataSort :: DataSort -> SDoc
ppDataSort data_sort = text $
  case data_sort of
    DataDeclSort     DataType -> "Data type"
    DataDeclSort     NewType  -> "Newtype"
    DataInstanceSort DataType -> "Data instance"
    DataInstanceSort NewType  -> "Newtype instance"
    DataFamilySort            -> "Data family"

-- | Helper type used in 'checkDataKindSig'.
--
-- Superficially similar to 'ContextKind', but it lacks 'AnyKind'
-- and 'AnyBoxedKind', and instead of @'TheKind' liftedTypeKind@
-- provides 'LiftedKind', which is much simpler to match on and
-- handle in 'isAllowedDataResKind'.
data AllowedDataResKind
  = AnyTYPEKind
  | AnyBoxedKind
  | LiftedKind

-- | A data type to describe why a variable is not closed.
-- See Note [Not-closed error messages] in GHC.Tc.Gen.Expr
data NotClosedReason = NotLetBoundReason
                     | NotTypeClosed VarSet
                     | NotClosed Name NotClosedReason

data SuggestPartialTypeSignatures
  = YesSuggestPartialTypeSignatures
  | NoSuggestPartialTypeSignatures
  deriving (Show, Eq)

suggestPartialTypeSignatures :: Bool -> SuggestPartialTypeSignatures
suggestPartialTypeSignatures True  = YesSuggestPartialTypeSignatures
suggestPartialTypeSignatures False = NoSuggestPartialTypeSignatures

data UsingGeneralizedNewtypeDeriving
  = YesGeneralizedNewtypeDeriving
  | NoGeneralizedNewtypeDeriving
  deriving Eq

usingGeneralizedNewtypeDeriving :: Bool -> UsingGeneralizedNewtypeDeriving
usingGeneralizedNewtypeDeriving True  = YesGeneralizedNewtypeDeriving
usingGeneralizedNewtypeDeriving False = NoGeneralizedNewtypeDeriving

data DeriveAnyClassEnabled
  = YesDeriveAnyClassEnabled
  | NoDeriveAnyClassEnabled
  deriving Eq

deriveAnyClassEnabled :: Bool -> DeriveAnyClassEnabled
deriveAnyClassEnabled True  = YesDeriveAnyClassEnabled
deriveAnyClassEnabled False = NoDeriveAnyClassEnabled

-- | Why a particular typeclass instance couldn't be derived.
data DeriveInstanceErrReason
  =
    -- | The typeclass instance is not well-kinded.
    DerivErrNotWellKinded !TyCon
                          -- ^ The type constructor that occurs in
                          -- the typeclass instance declaration.
                          !Kind
                          -- ^ The typeclass kind.
                          !Int
                          -- ^ The number of typeclass arguments that GHC
                          -- kept. See Note [tc_args and tycon arity] in
                          -- GHC.Tc.Deriv.
  -- | Generic instances can only be derived using the stock strategy
  -- in Safe Haskell.
  | DerivErrSafeHaskellGenericInst
  | DerivErrDerivingViaWrongKind !Kind !Type !Kind
  | DerivErrNoEtaReduce !Type
                        -- ^ The instance type
  -- | We cannot derive instances in boot files
  | DerivErrBootFileFound
  | DerivErrDataConsNotAllInScope !TyCon
  -- | We cannot use GND on non-newtype types
  | DerivErrGNDUsedOnData
  -- | We cannot derive instances of nullary classes
  | DerivErrNullaryClasses
  -- | Last arg must be newtype or data application
  | DerivErrLastArgMustBeApp
  | DerivErrNoFamilyInstance !TyCon [Type]
  | DerivErrNotStockDeriveable !DeriveAnyClassEnabled
  | DerivErrHasAssociatedDatatypes !HasAssociatedDataFamInsts
                                   !AssociatedTyLastVarInKind
                                   !AssociatedTyNotParamOverLastTyVar
  | DerivErrNewtypeNonDeriveableClass
  | DerivErrCannotEtaReduceEnough !Bool -- Is eta-reduction OK?
  | DerivErrOnlyAnyClassDeriveable !TyCon
                                   -- ^ Type constructor for which the instance
                                   -- is requested
                                   !DeriveAnyClassEnabled
                                   -- ^ Whether or not -XDeriveAnyClass is enabled
                                   -- already.
  -- | Stock deriving won't work, but perhaps DeriveAnyClass will.
  | DerivErrNotDeriveable !DeriveAnyClassEnabled
  -- | The given 'PredType' is not a class.
  | DerivErrNotAClass !PredType
  -- | The given (representation of the) 'TyCon' has no
  -- data constructors.
  | DerivErrNoConstructors !TyCon
  | DerivErrLangExtRequired !LangExt.Extension
  -- | GHC simply doesn't how to how derive the input 'Class' for the given
  -- 'Type'.
  | DerivErrDunnoHowToDeriveForType !Type
  -- | The given 'TyCon' must be an enumeration.
  -- See Note [Enumeration types] in GHC.Core.TyCon
  | DerivErrMustBeEnumType !TyCon
  -- | The given 'TyCon' must have /precisely/ one constructor.
  | DerivErrMustHaveExactlyOneConstructor !TyCon
  -- | The given data type must have some parameters.
  | DerivErrMustHaveSomeParameters !TyCon
  -- | The given data type must not have a class context.
  | DerivErrMustNotHaveClassContext !TyCon !ThetaType
  -- | We couldn't derive an instance for a particular data constructor
  -- for a variety of reasons.
  | DerivErrBadConstructor !(Maybe HasWildcard) [DeriveInstanceBadConstructor]
  -- | We couldn't derive a 'Generic' instance for the given type for a
  -- variety of reasons
  | DerivErrGenerics [DeriveGenericsErrReason]
  -- | We couldn't derive an instance either because the type was not an
  -- enum type or because it did have more than one constructor.
  | DerivErrEnumOrProduct !DeriveInstanceErrReason !DeriveInstanceErrReason
  deriving Generic

data DeriveInstanceBadConstructor
  =
  -- | The given 'DataCon' must be truly polymorphic in the
  -- last argument of the data type.
    DerivErrBadConExistential !DataCon
  -- | The given 'DataCon' must not use the type variable in a function argument"
  | DerivErrBadConCovariant !DataCon
  -- | The given 'DataCon' must not contain function types
  | DerivErrBadConFunTypes !DataCon
  -- | The given 'DataCon' must use the type variable only
  -- as the last argument of a data type
  | DerivErrBadConWrongArg !DataCon
  -- | The given 'DataCon' is a GADT so we cannot directly
  -- derive an istance for it.
  | DerivErrBadConIsGADT !DataCon
  -- | The given 'DataCon' has existentials type vars in its type.
  | DerivErrBadConHasExistentials !DataCon
  -- | The given 'DataCon' has constraints in its type.
  | DerivErrBadConHasConstraints !DataCon
  -- | The given 'DataCon' has a higher-rank type.
  | DerivErrBadConHasHigherRankType !DataCon

data DeriveGenericsErrReason
  = -- | The type must not have some datatype context.
    DerivErrGenericsMustNotHaveDatatypeContext !TyCon
    -- | The data constructor must not have exotic unlifted
    -- or polymorphic arguments.
  | DerivErrGenericsMustNotHaveExoticArgs !DataCon
    -- | The data constructor must be a vanilla constructor.
  | DerivErrGenericsMustBeVanillaDataCon  !DataCon
    -- | The type must have some type parameters.
    -- check (d) from Note [Requirements for deriving Generic and Rep]
    -- in GHC.Tc.Deriv.Generics.
  | DerivErrGenericsMustHaveSomeTypeParams !TyCon
    -- | The data constructor must not have existential arguments.
  | DerivErrGenericsMustNotHaveExistentials !DataCon
    -- | The derivation applies a type to an argument involving
    -- the last parameter but the applied type is not of kind * -> *.
  | DerivErrGenericsWrongArgKind !DataCon

data HasWildcard
  = YesHasWildcard
  | NoHasWildcard
  deriving Eq

hasWildcard :: Bool -> HasWildcard
hasWildcard True  = YesHasWildcard
hasWildcard False = NoHasWildcard

-- | A context in which we don't allow anonymous wildcards.
data BadAnonWildcardContext
  = WildcardNotLastInConstraint
  | ExtraConstraintWildcardNotAllowed
      SoleExtraConstraintWildcardAllowed
  | WildcardsNotAllowedAtAll

-- | Whether a sole extra-constraint wildcard is allowed,
-- e.g. @_ => ..@ as opposed to @( .., _ ) => ..@.
data SoleExtraConstraintWildcardAllowed
  = SoleExtraConstraintWildcardNotAllowed
  | SoleExtraConstraintWildcardAllowed

-- | A type representing whether or not the input type has associated data family instances.
data HasAssociatedDataFamInsts
  = YesHasAdfs
  | NoHasAdfs
  deriving Eq

hasAssociatedDataFamInsts :: Bool -> HasAssociatedDataFamInsts
hasAssociatedDataFamInsts True = YesHasAdfs
hasAssociatedDataFamInsts False = NoHasAdfs

-- | If 'YesAssocTyLastVarInKind', the associated type of a typeclass
-- contains the last type variable of the class in a kind, which is not (yet) allowed
-- by GHC.
data AssociatedTyLastVarInKind
  = YesAssocTyLastVarInKind !TyCon -- ^ The associated type family of the class
  | NoAssocTyLastVarInKind
  deriving Eq

associatedTyLastVarInKind :: Maybe TyCon -> AssociatedTyLastVarInKind
associatedTyLastVarInKind (Just tc) = YesAssocTyLastVarInKind tc
associatedTyLastVarInKind Nothing   = NoAssocTyLastVarInKind

-- | If 'NoAssociatedTyNotParamOverLastTyVar', the associated type of a
-- typeclass is not parameterized over the last type variable of the class
data AssociatedTyNotParamOverLastTyVar
  = YesAssociatedTyNotParamOverLastTyVar !TyCon -- ^ The associated type family of the class
  | NoAssociatedTyNotParamOverLastTyVar
  deriving Eq

associatedTyNotParamOverLastTyVar :: Maybe TyCon -> AssociatedTyNotParamOverLastTyVar
associatedTyNotParamOverLastTyVar (Just tc) = YesAssociatedTyNotParamOverLastTyVar tc
associatedTyNotParamOverLastTyVar Nothing   = NoAssociatedTyNotParamOverLastTyVar

-- | What kind of thing is missing a type signature?
--
-- Used for reporting @"missing signature"@ warnings, see
-- 'tcRnMissingSignature'.
data MissingSignature
  = MissingTopLevelBindingSig Name Type
  | MissingPatSynSig PatSyn
  | MissingTyConKindSig
      TyCon
      Bool -- ^ whether -XCUSKs is enabled

-- | Is the object we are dealing with exported or not?
--
-- Used for reporting @"missing signature"@ warnings, see
-- 'TcRnMissingSignature'.
data Exported
  = IsNotExported
  | IsExported

instance Outputable Exported where
  ppr IsNotExported = text "IsNotExported"
  ppr IsExported    = text "IsExported"


--------------------------------------------------------------------------------
--
--     Errors used in GHC.Tc.Errors
--
--------------------------------------------------------------------------------

{- Note [Error report]
~~~~~~~~~~~~~~~~~~~~~~
The idea is that error msgs are divided into three parts: the main msg, the
context block ("In the second argument of ..."), and the relevant bindings
block, which are displayed in that order, with a mark to divide them. The
the main msg ('report_important') varies depending on the error
in question, but context and relevant bindings are always the same, which
should simplify visual parsing.

See 'GHC.Tc.Errors.Types.SolverReport' and 'GHC.Tc.Errors.mkErrorReport'.
-}

-- | A collection of main error messages and supplementary information.
--
-- In practice, we will:
--  - display the important messages first,
--  - then the error context (e.g. by way of a call to 'GHC.Tc.Errors.mkErrorReport'),
--  - then the supplementary information (e.g. relevant bindings, valid hole fits),
--  - then the hints ("Possible fix: ...").
--
-- So this is mostly just a way of making sure that the error context appears
-- early on rather than at the end of the message.
--
-- See Note [Error report] for details.
data SolverReport
  = SolverReport
  { sr_important_msg :: SolverReportWithCtxt
  , sr_supplementary :: [SolverReportSupplementary]
  , sr_hints         :: [GhcHint]
  }

-- | Additional information to print in a 'SolverReport', after the
-- important messages and after the error context.
--
-- See Note [Error report].
data SolverReportSupplementary
  = SupplementaryBindings RelevantBindings
  | SupplementaryHoleFits ValidHoleFits
  | SupplementaryCts      [(PredType, RealSrcSpan)]

-- | A 'TcSolverReportMsg', together with context (e.g. enclosing implication constraints)
-- that are needed in order to report it.
data SolverReportWithCtxt =
  SolverReportWithCtxt
    { reportContext :: SolverReportErrCtxt
       -- ^ Context for what we wish to report.
       -- This can change as we enter implications, so is
       -- stored alongside the content.
    , reportContent :: TcSolverReportMsg
      -- ^ The content of the message to report.
    }
  deriving Generic

-- | Context needed when reporting a 'TcSolverReportMsg', such as
-- the enclosing implication constraints or whether we are deferring type errors.
data SolverReportErrCtxt
    = CEC { cec_encl :: [Implication]  -- ^ Enclosing implications
                                       --   (innermost first)
                                       -- ic_skols and givens are tidied, rest are not
          , cec_tidy  :: TidyEnv

          , cec_binds :: EvBindsVar    -- ^ We make some errors (depending on cec_defer)
                                       -- into warnings, and emit evidence bindings
                                       -- into 'cec_binds' for unsolved constraints

          , cec_defer_type_errors :: DiagnosticReason -- ^ Whether to defer type errors until runtime

          -- We might throw a warning on an error when encountering a hole,
          -- depending on the type of hole (expression hole, type hole, out of scope hole).
          -- We store the reasons for reporting a diagnostic for each type of hole.
          , cec_expr_holes :: DiagnosticReason -- ^ Reason for reporting holes in expressions.
          , cec_type_holes :: DiagnosticReason -- ^ Reason for reporting holes in types.
          , cec_out_of_scope_holes :: DiagnosticReason -- ^ Reason for reporting out of scope holes.

          , cec_warn_redundant :: Bool    -- ^ True <=> -Wredundant-constraints
          , cec_expand_syns    :: Bool    -- ^ True <=> -fprint-expanded-synonyms

          , cec_suppress :: Bool    -- ^ True <=> More important errors have occurred,
                                    --            so create bindings if need be, but
                                    --            don't issue any more errors/warnings
                                    -- See Note [Suppressing error messages]
      }

getUserGivens :: SolverReportErrCtxt -> [UserGiven]
-- One item for each enclosing implication
getUserGivens (CEC {cec_encl = implics}) = getUserGivensFromImplics implics

----------------------------------------------------------------------------
--
--   ErrorItem
--
----------------------------------------------------------------------------

-- | A predicate with its arising location; used to encapsulate a constraint
-- that will give rise to a diagnostic.
data ErrorItem
-- We could perhaps use Ct here (and indeed used to do exactly that), but
-- having a separate type gives to denote errors-in-formation gives us
-- a nice place to do pre-processing, such as calculating ei_suppress.
-- Perhaps some day, an ErrorItem could eventually evolve to contain
-- the error text (or some representation of it), so we can then have all
-- the errors together when deciding which to report.
  = EI { ei_pred     :: PredType         -- report about this
         -- The ei_pred field will never be an unboxed equality with
         -- a (casted) tyvar on the right; this is guaranteed by the solver
       , ei_evdest   :: Maybe TcEvDest   -- for Wanteds, where to put evidence
       , ei_flavour  :: CtFlavour
       , ei_loc      :: CtLoc
       , ei_m_reason :: Maybe CtIrredReason  -- if this ErrorItem was made from a
                                             -- CtIrred, this stores the reason
       , ei_suppress :: Bool    -- Suppress because of Note [Wanteds rewrite Wanteds]
                                -- in GHC.Tc.Constraint
       }

instance Outputable ErrorItem where
  ppr (EI { ei_pred     = pred
          , ei_evdest   = m_evdest
          , ei_flavour  = flav
          , ei_suppress = supp })
    = pp_supp <+> ppr flav <+> pp_dest m_evdest <+> ppr pred
    where
      pp_dest Nothing   = empty
      pp_dest (Just ev) = ppr ev <+> dcolon

      pp_supp = if supp then text "suppress:" else empty

errorItemOrigin :: ErrorItem -> CtOrigin
errorItemOrigin = ctLocOrigin . ei_loc

errorItemEqRel :: ErrorItem -> EqRel
errorItemEqRel = predTypeEqRel . ei_pred

errorItemCtLoc :: ErrorItem -> CtLoc
errorItemCtLoc = ei_loc

errorItemPred :: ErrorItem -> PredType
errorItemPred = ei_pred

{- Note [discardProvCtxtGivens]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In most situations we call all enclosing implications "useful". There is one
exception, and that is when the constraint that causes the error is from the
"provided" context of a pattern synonym declaration:

  pattern Pat :: (Num a, Eq a) => Show a   => a -> Maybe a
             --  required      => provided => type
  pattern Pat x <- (Just x, 4)

When checking the pattern RHS we must check that it does actually bind all
the claimed "provided" constraints; in this case, does the pattern (Just x, 4)
bind the (Show a) constraint.  Answer: no!

But the implication we generate for this will look like
   forall a. (Num a, Eq a) => [W] Show a
because when checking the pattern we must make the required
constraints available, since they are needed to match the pattern (in
this case the literal '4' needs (Num a, Eq a)).

BUT we don't want to suggest adding (Show a) to the "required" constraints
of the pattern synonym, thus:
  pattern Pat :: (Num a, Eq a, Show a) => Show a => a -> Maybe a
It would then typecheck but it's silly.  We want the /pattern/ to bind
the alleged "provided" constraints, Show a.

So we suppress that Implication in discardProvCtxtGivens.  It's
painfully ad-hoc but the truth is that adding it to the "required"
constraints would work.  Suppressing it solves two problems.  First,
we never tell the user that we could not deduce a "provided"
constraint from the "required" context. Second, we never give a
possible fix that suggests to add a "provided" constraint to the
"required" context.

For example, without this distinction the above code gives a bad error
message (showing both problems):

  error: Could not deduce (Show a) ... from the context: (Eq a)
         ... Possible fix: add (Show a) to the context of
         the signature for pattern synonym `Pat' ...
-}


discardProvCtxtGivens :: CtOrigin -> [UserGiven] -> [UserGiven]
discardProvCtxtGivens orig givens  -- See Note [discardProvCtxtGivens]
  | ProvCtxtOrigin (PSB {psb_id = L _ name}) <- orig
  = filterOut (discard name) givens
  | otherwise
  = givens
  where
    discard n (Implic { ic_info = SigSkol (PatSynCtxt n') _ _ }) = n == n'
    discard _ _                                                  = False


-- | An error reported after constraint solving.
-- This is usually, some sort of unsolved constraint error,
-- but we try to be specific about the precise problem we encountered.
data TcSolverReportMsg
  -- | Quantified variables appear out of dependency order.
  --
  -- Example:
  --
  --   forall (a :: k) k. ...
  --
  -- Test cases: BadTelescope2, T16418, T16247, T16726, T18451.
  = BadTelescope TyVarBndrs [TyCoVar]

  -- | We came across a custom type error and we have decided to report it.
  --
  -- Example:
  --
  --   type family F a where
  --     F a = TypeError (Text "error")
  --
  --   err :: F ()
  --   err = ()
  --
  -- Test cases: CustomTypeErrors0{1,2,3,4,5}, T12104.
  | UserTypeError Type

  -- | We want to report an out of scope variable or a typed hole.
  -- See 'HoleError'.
  | ReportHoleError Hole HoleError

  -- | Trying to unify an untouchable variable, e.g. a variable from an outer scope.
  --
  -- Test case: Simple14
  | UntouchableVariable
    { untouchableTyVar :: TyVar
    , untouchableTyVarImplication :: Implication
    }

  -- | Cannot unify a variable, because of a type mismatch.
  | CannotUnifyVariable
    { mismatchMsg         :: MismatchMsg
    , cannotUnifyReason   :: CannotUnifyVariableReason }

  -- | A mismatch between two types.
  | Mismatch
     { mismatchMsg           :: MismatchMsg
     , mismatchTyVarInfo     :: Maybe TyVarInfo
     , mismatchAmbiguityInfo :: [AmbiguityInfo]
     , mismatchCoercibleInfo :: Maybe CoercibleMsg }

   -- | A violation of the representation-polymorphism invariants.
   --
   -- See 'FixedRuntimeRepErrorInfo' and 'FixedRuntimeRepContext' for more information.
  | FixedRuntimeRepError [FixedRuntimeRepErrorInfo]

  -- | An equality between two types is blocked on a kind equality
  -- between their kinds.
  --
  -- Test cases: none.
  | BlockedEquality ErrorItem
    -- These are for the "blocked" equalities, as described in
    -- Note [Equalities with incompatible kinds] in GHC.Tc.Solver.Canonical,
    -- wrinkle (2). There should always be another unsolved wanted around,
    -- which will ordinarily suppress this message. But this can still be printed out
    -- with -fdefer-type-errors (sigh), so we must produce a message.

  -- | Something was not applied to sufficiently many arguments.
  --
  --  Example:
  --
  --    instance Eq Maybe where {..}
  --
  -- Test case: T11563.
  | ExpectingMoreArguments Int TypedThing

  -- | Trying to use an unbound implicit parameter.
  --
  -- Example:
  --
  --    foo :: Int
  --    foo = ?param
  --
  -- Test case: tcfail130.
  | UnboundImplicitParams
      (NE.NonEmpty ErrorItem)

  -- | A constraint couldn't be solved because it contains
  -- ambiguous type variables.
  --
  -- Example:
  --
  --   class C a b where
  --     f :: (a,b)
  --
  --   x = fst f
  --
  --
  -- Test case: T4921.
  | AmbiguityPreventsSolvingCt
      ErrorItem -- ^ always a class constraint
      ([TyVar], [TyVar]) -- ^ ambiguous kind and type variables, respectively

  -- | Could not solve a constraint; there were several unifying candidate instances
  -- but no matching instances. This is used to report as much useful information
  -- as possible about why we couldn't choose any instance, e.g. because of
  -- ambiguous type variables.
  | CannotResolveInstance
    { cannotResolve_item         :: ErrorItem
    , cannotResolve_unifiers     :: [ClsInst]
    , cannotResolve_candidates   :: [ClsInst]
    , cannotResolve_importErrors :: [ImportError]
    , cannotResolve_suggestions  :: [GhcHint]
    , cannotResolve_relevant_bindings :: RelevantBindings }
      -- TODO: remove the fields of type [GhcHint] and RelevantBindings,
      -- in order to handle them uniformly with other diagnostic messages.

  -- | Could not solve a constraint using available instances
  -- because the instances overlap.
  --
  -- Test cases: tcfail118, tcfail121, tcfail218.
  | OverlappingInstances
    { overlappingInstances_item     :: ErrorItem
    , overlappingInstances_matches  :: NE.NonEmpty ClsInst
    , overlappingInstances_unifiers :: [ClsInst] }

  -- | Could not solve a constraint from instances because
  -- instances declared in a Safe module cannot overlap instances
  -- from other modules (with -XSafeHaskell).
  --
  -- Test cases: SH_Overlap{1,2,5,6,7,11}.
  | UnsafeOverlap
    { unsafeOverlap_item    :: ErrorItem
    , unsafeOverlap_match   :: ClsInst
    , unsafeOverlapped      :: NE.NonEmpty ClsInst }

  deriving Generic

data MismatchMsg
  =  -- | Couldn't unify two types or kinds.
  --
  --  Example:
  --
  --    3 + 3# -- can't match a lifted type with an unlifted type
  --
  --  Test cases: T1396, T8263, ...
    BasicMismatch
      { mismatch_ea           :: MismatchEA  -- ^ Should this be phrased in terms of expected vs actual?
      , mismatch_item         :: ErrorItem   -- ^ The constraint in which the mismatch originated.
      , mismatch_ty1          :: Type        -- ^ First type (the expected type if if mismatch_ea is True)
      , mismatch_ty2          :: Type        -- ^ Second type (the actual type if mismatch_ea is True)
      , mismatch_whenMatching :: Maybe WhenMatching
      , mismatch_mb_same_occ  :: Maybe SameOccInfo
      }

  -- | A type has an unexpected kind.
  --
  -- Test cases: T2994, T7609, ...
  | KindMismatch
      { kmismatch_what     :: TypedThing -- ^ What thing is 'kmismatch_actual' the kind of?
      , kmismatch_expected :: Type
      , kmismatch_actual   :: Type
      }
    -- TODO: combine with 'BasicMismatch'.

  -- | A mismatch between two types, which arose from a type equality.
  --
  -- Test cases: T1470, tcfail212.
  | TypeEqMismatch
      { teq_mismatch_ppr_explicit_kinds :: Bool
      , teq_mismatch_item     :: ErrorItem
      , teq_mismatch_ty1      :: Type
      , teq_mismatch_ty2      :: Type
      , teq_mismatch_expected :: Type -- ^ The overall expected type
      , teq_mismatch_actual   :: Type -- ^ The overall actual type
      , teq_mismatch_what     :: Maybe TypedThing -- ^ What thing is 'teq_mismatch_actual' the kind of?
      , teq_mb_same_occ       :: Maybe SameOccInfo
      }
    -- TODO: combine with 'BasicMismatch'.

  -- | Couldn't solve some Wanted constraints using the Givens.
  -- Used for messages such as @"No instance for ..."@ and
  -- @"Could not deduce ... from"@.
  | CouldNotDeduce
     { cnd_user_givens :: [Implication]
        -- | The Wanted constraints we couldn't solve.
        --
        -- N.B.: the 'ErrorItem' at the head of the list has been tidied,
        -- perhaps not the others.
     , cnd_wanted      :: NE.NonEmpty ErrorItem

       -- | Some additional info consumed by 'mk_supplementary_ea_msg'.
     , cnd_extra       :: Maybe CND_Extra
     }
  deriving Generic

-- | Construct a basic mismatch message between two types.
--
-- See 'pprMismatchMsg' for how such a message is displayed to users.
mkBasicMismatchMsg :: MismatchEA -> ErrorItem -> Type -> Type -> MismatchMsg
mkBasicMismatchMsg ea item ty1 ty2
  = BasicMismatch
      { mismatch_ea           = ea
      , mismatch_item         = item
      , mismatch_ty1          = ty1
      , mismatch_ty2          = ty2
      , mismatch_whenMatching = Nothing
      , mismatch_mb_same_occ  = Nothing
      }

-- | Whether to use expected/actual in a type mismatch message.
data MismatchEA
  -- | Don't use expected/actual.
  = NoEA
  -- | Use expected/actual.
  | EA
  { mismatch_mbEA :: Maybe ExpectedActualInfo
    -- ^ Whether to also mention type synonym expansion.
  }

data CannotUnifyVariableReason
  =  -- | A type equality between a type variable and a polytype.
    --
    -- Test cases: T12427a, T2846b, T10194, ...
    CannotUnifyWithPolytype ErrorItem TyVar Type (Maybe TyVarInfo)

  -- | An occurs check.
  | OccursCheck
    { occursCheckInterestingTyVars :: [TyVar]
    , occursCheckAmbiguityInfos    :: [AmbiguityInfo] }

  -- | A skolem type variable escapes its scope.
  --
  -- Example:
  --
  --   data Ex where { MkEx :: a -> MkEx }
  --   foo (MkEx x) = x
  --
  -- Test cases: TypeSkolEscape, T11142.
  | SkolemEscape ErrorItem Implication [TyVar]

  -- | Can't unify the type variable with the other type
  -- due to the kind of type variable it is.
  --
  -- For example, trying to unify a 'SkolemTv' with the
  -- type Int, or with a 'TyVarTv'.
  | DifferentTyVars TyVarInfo
  | RepresentationalEq TyVarInfo (Maybe CoercibleMsg)
  deriving Generic

-- | Report a mismatch error without any extra
-- information.
mkPlainMismatchMsg :: MismatchMsg -> TcSolverReportMsg
mkPlainMismatchMsg msg
  = Mismatch
     { mismatchMsg           = msg
     , mismatchTyVarInfo     = Nothing
     , mismatchAmbiguityInfo = []
     , mismatchCoercibleInfo = Nothing }

-- | Additional information to be given in a 'CouldNotDeduce' message,
-- which is then passed on to 'mk_supplementary_ea_msg'.
data CND_Extra = CND_Extra TypeOrKind Type Type

-- | A cue to print out information about type variables,
-- e.g. where they were bound, when there is a mismatch @tv1 ~ ty2@.
data TyVarInfo =
  TyVarInfo { thisTyVar :: TyVar
            , thisTyVarIsUntouchable :: Maybe Implication
            , otherTy   :: Maybe TyVar }

-- | Add some information to disambiguate errors in which
-- two 'Names' would otherwise appear to be identical.
--
-- See Note [Disambiguating (X ~ X) errors].
data SameOccInfo
  = SameOcc
    { sameOcc_same_pkg :: Bool -- ^ Whether the two 'Name's also came from the same package.
    , sameOcc_lhs :: Name
    , sameOcc_rhs :: Name }

-- | Add some information about ambiguity
data AmbiguityInfo

  -- | Some type variables remained ambiguous: print them to the user.
  = Ambiguity
    { lead_with_ambig_msg :: Bool -- ^ True <=> start the message with "Ambiguous type variable ..."
                                  --  False <=> create a message of the form "The type variable is ambiguous."
    , ambig_tyvars        :: ([TyVar], [TyVar]) -- ^ Ambiguous kind and type variables, respectively.
                                                -- Guaranteed to not both be empty.
    }

  -- | Remind the user that a particular type family is not injective.
  | NonInjectiveTyFam TyCon

-- | Expected/actual information.
data ExpectedActualInfo
  -- | Display the expected and actual types.
  = ExpectedActual
     { ea_expected, ea_actual :: Type }

  -- | Display the expected and actual types, after expanding type synonyms.
  | ExpectedActualAfterTySynExpansion
     { ea_expanded_expected, ea_expanded_actual :: Type }

-- | Explain how a kind equality originated.
data WhenMatching

  = WhenMatching TcType TcType CtOrigin (Maybe TypeOrKind)
  deriving Generic

-- | Some form of @"not in scope"@ error. See also the 'OutOfScopeHole'
-- constructor of 'HoleError'.
data NotInScopeError

  -- | A run-of-the-mill @"not in scope"@ error.
  = NotInScope

  -- | Something used in record syntax, but it isn't a record field.
  | NotARecordField
    -- TODO: this could be folded into NotInScope were there
    -- a separate namespace for record fields.

  -- | An exact 'Name' was not in scope.
  --
  -- This usually indicates a problem with a Template Haskell splice.
  --
  -- Test cases: T5971, T18263.
  | NoExactName Name

  -- The same exact 'Name' occurs in multiple name-spaces.
  --
  -- This usually indicates a problem with a Template Haskell splice.
  --
  -- Test case: T7241.
  | SameName [GlobalRdrElt] -- ^ always at least 2 elements

  -- A type signature, fixity declaration, pragma, standalone kind signature...
  -- is missing an associated binding.
  | MissingBinding SDoc [GhcHint]
    -- TODO: remove the SDoc argument.

  -- | Couldn't find a top-level binding.
  --
  -- Happens when specifying an annotation for something that
  -- is not in scope.
  --
  -- Test cases: annfail01, annfail02, annfail11.
  | NoTopLevelBinding

  -- | A class doesn't have a method with this name,
  -- or, a class doesn't have an associated type with this name,
  -- or, a record doesn't have a record field with this name.
  | UnknownSubordinate SDoc

  -- | A name is not in scope during type checking but passed the renamer.
  --
  -- Test cases:
  --   none
  | NotInScopeTc (NameEnv TcTyThing)
  deriving Generic

-- | Create a @"not in scope"@ error message for the given 'RdrName'.
mkTcRnNotInScope :: RdrName -> NotInScopeError -> TcRnMessage
mkTcRnNotInScope rdr err = TcRnNotInScope err rdr [] noHints

-- | Configuration for pretty-printing valid hole fits.
data HoleFitDispConfig =
  HFDC { showWrap, showWrapVars, showType, showProv, showMatches
          :: Bool }

-- | Report an error involving a 'Hole'.
--
-- This could be an out of scope data constructor or variable,
-- a typed hole, or a wildcard in a type.
data HoleError
  -- | Report an out-of-scope data constructor or variable
  -- masquerading as an expression hole.
  --
  -- See Note [Insoluble holes] in GHC.Tc.Types.Constraint.
  -- See 'NotInScopeError' for other not-in-scope errors.
  --
  -- Test cases: T9177a.
  = OutOfScopeHole [ImportError]
  -- | Report a typed hole, or wildcard, with additional information.
  | HoleError HoleSort
              [TcTyVar]                     -- Other type variables which get computed on the way.
              [(SkolemInfoAnon, [TcTyVar])] -- Zonked and grouped skolems for the type of the hole.

-- | A message that aims to explain why two types couldn't be seen
-- to be representationally equal.
data CoercibleMsg
  -- | Not knowing the role of a type constructor prevents us from
  -- concluding that two types are representationally equal.
  --
  -- Example:
  --
  --   foo :: Applicative m => m (Sum Int)
  --   foo = coerce (pure $ 1 :: Int)
  --
  -- We don't know what role `m` has, so we can't coerce `m Int` to `m (Sum Int)`.
  --
  -- Test cases: T8984, TcCoercibleFail.
  = UnknownRoles Type

  -- | The fact that a 'TyCon' is abstract prevents us from decomposing
  -- a 'TyConApp' and deducing that two types are representationally equal.
  --
  -- Test cases: none.
  | TyConIsAbstract TyCon

  -- | We can't unwrap a newtype whose constructor is not in scope.
  --
  -- Example:
  --
  --   import Data.Ord (Down) -- NB: not importing the constructor
  --   foo :: Int -> Down Int
  --   foo = coerce
  --
  -- Test cases: TcCoercibleFail.
  | OutOfScopeNewtypeConstructor TyCon DataCon

-- | Explain a problem with an import.
data ImportError
  -- | Couldn't find a module with the requested name.
  = MissingModule ModuleName
  -- | The imported modules don't export what we're looking for.
  | ModulesDoNotExport (NE.NonEmpty Module) OccName

-- | This datatype collates instances that match or unifier,
-- in order to report an error message for an unsolved typeclass constraint.
data PotentialInstances
  = PotentialInstances
  { matches  :: [ClsInst]
  , unifiers :: [ClsInst]
  }

-- | A collection of valid hole fits or refinement fits,
-- in which some fits might have been suppressed.
data FitsMbSuppressed
  = Fits
    { fits           :: [HoleFit]
    , fitsSuppressed :: Bool  -- ^ Whether we have suppressed any fits because there were too many.
    }

-- | A collection of hole fits and refinement fits.
data ValidHoleFits
  = ValidHoleFits
    { holeFits       :: FitsMbSuppressed
    , refinementFits :: FitsMbSuppressed
    }

noValidHoleFits :: ValidHoleFits
noValidHoleFits = ValidHoleFits (Fits [] False) (Fits [] False)

data RelevantBindings
  = RelevantBindings
    { relevantBindingNamesAndTys :: [(Name, Type)]
    , ranOutOfFuel               :: Bool -- ^ Whether we ran out of fuel generating the bindings.
    }

-- | Display some relevant bindings.
pprRelevantBindings :: RelevantBindings -> SDoc
-- This function should be in "GHC.Tc.Errors.Ppr",
-- but it's here for the moment as it's needed in "GHC.Tc.Errors".
pprRelevantBindings (RelevantBindings bds ran_out_of_fuel) =
  ppUnless (null rel_bds) $
    hang (text "Relevant bindings include")
       2 (vcat (map ppr_binding rel_bds) $$ ppWhen ran_out_of_fuel discardMsg)
  where
    ppr_binding (nm, tidy_ty) =
      sep [ pprPrefixOcc nm <+> dcolon <+> ppr tidy_ty
          , nest 2 (parens (text "bound at"
               <+> ppr (getSrcLoc nm)))]
    rel_bds = filter (not . isGeneratedSrcSpan . getSrcSpan . fst) bds

discardMsg :: SDoc
discardMsg = text "(Some bindings suppressed;" <+>
             text "use -fmax-relevant-binds=N or -fno-max-relevant-binds)"

data PromotionErr
  = TyConPE          -- TyCon used in a kind before we are ready
                     --     data T :: T -> * where ...
  | ClassPE          -- Ditto Class

  | FamDataConPE     -- Data constructor for a data family
                     -- See Note [AFamDataCon: not promoting data family constructors]
                     -- in GHC.Tc.Utils.Env.
  | ConstrainedDataConPE ThetaType -- Data constructor with a context
                                   -- See Note [No constraints in kinds] in GHC.Tc.Validity
  | PatSynPE         -- Pattern synonyms
                     -- See Note [Don't promote pattern synonyms] in GHC.Tc.Utils.Env

  | RecDataConPE     -- Data constructor in a recursive loop
                     -- See Note [Recursion and promoting data constructors] in GHC.Tc.TyCl
  | TermVariablePE   -- See Note [Promoted variables in types]
  | NoDataKindsDC    -- -XDataKinds not enabled (for a datacon)

instance Outputable PromotionErr where
  ppr ClassPE              = text "ClassPE"
  ppr TyConPE              = text "TyConPE"
  ppr PatSynPE             = text "PatSynPE"
  ppr FamDataConPE         = text "FamDataConPE"
  ppr (ConstrainedDataConPE theta) = text "ConstrainedDataConPE" <+> parens (ppr theta)
  ppr RecDataConPE         = text "RecDataConPE"
  ppr NoDataKindsDC        = text "NoDataKindsDC"
  ppr TermVariablePE       = text "TermVariablePE"

pprPECategory :: PromotionErr -> SDoc
pprPECategory = text . capitalise . peCategory

peCategory :: PromotionErr -> String
peCategory ClassPE              = "class"
peCategory TyConPE              = "type constructor"
peCategory PatSynPE             = "pattern synonym"
peCategory FamDataConPE         = "data constructor"
peCategory ConstrainedDataConPE{} = "data constructor"
peCategory RecDataConPE         = "data constructor"
peCategory NoDataKindsDC        = "data constructor"
peCategory TermVariablePE       = "term variable"

-- | Stores the information to be reported in a representation-polymorphism
-- error message.
data FixedRuntimeRepErrorInfo
  = FRR_Info
  { frr_info_origin       :: FixedRuntimeRepOrigin
      -- ^ What is the original type we checked for
      -- representation-polymorphism, and what specific
      -- check did we perform?
  , frr_info_not_concrete :: Maybe (TcTyVar, TcType)
      -- ^ Which non-concrete type did we try to
      -- unify this concrete type variable with?
  }

{-
************************************************************************
*                                                                      *
\subsection{Contexts for renaming errors}
*                                                                      *
************************************************************************
-}

-- AZ:TODO: Change these all to be Name instead of RdrName.
--          Merge TcType.UserTypeContext in to it.
data HsDocContext
  = TypeSigCtx SDoc
  | StandaloneKindSigCtx SDoc
  | PatCtx
  | SpecInstSigCtx
  | DefaultDeclCtx
  | ForeignDeclCtx (LocatedN RdrName)
  | DerivDeclCtx
  | RuleCtx FastString
  | TyDataCtx (LocatedN RdrName)
  | TySynCtx (LocatedN RdrName)
  | TyFamilyCtx (LocatedN RdrName)
  | FamPatCtx (LocatedN RdrName)    -- The patterns of a type/data family instance
  | ConDeclCtx [LocatedN Name]
  | ClassDeclCtx (LocatedN RdrName)
  | ExprWithTySigCtx
  | TypBrCtx
  | HsTypeCtx
  | HsTypePatCtx
  | GHCiCtx
  | SpliceTypeCtx (LHsType GhcPs)
  | ClassInstanceCtx
  | GenericCtx SDoc

-- | Context for a mismatch in the number of arguments
data MatchArgsContext
  = EquationArgs
      !Name -- ^ Name of the function
  | PatternArgs
      !(HsMatchContext GhcTc) -- ^ Pattern match specifics

-- | The information necessary to report mismatched
-- numbers of arguments in a match group.
data MatchArgBadMatches where
  MatchArgMatches
    ::  { matchArgFirstMatch :: LocatedA (Match GhcRn body)
        , matchArgBadMatches :: NE.NonEmpty (LocatedA (Match GhcRn body)) }
    -> MatchArgBadMatches

-- | The phase in which an exception was encountered when dealing with a TH splice
data SplicePhase
  = SplicePhase_Run
  | SplicePhase_CompileAndLink

data LookupTHInstNameErrReason
  = NoMatchesFound
  | CouldNotDetermineInstance

data UnrepresentableTypeDescr
  = LinearInvisibleArgument
  | CoercionsInTypes

-- | The context for an "empty statement group" error.
data EmptyStatementGroupErrReason
  = EmptyStmtsGroupInParallelComp
  -- ^ Empty statement group in a parallel list comprehension
  | EmptyStmtsGroupInTransformListComp
  -- ^ Empty statement group in a transform list comprehension
  --
  --   Example:
  --   [() | then ()]
  | EmptyStmtsGroupInDoNotation HsDoFlavour
  -- ^ Empty statement group in do notation
  --
  --   Example:
  --   do
  | EmptyStmtsGroupInArrowNotation
  -- ^ Empty statement group in arrow notation
  --
  --   Example:
  --   proc () -> do

  deriving (Generic)

-- | An existential wrapper around @'StmtLR' GhcPs GhcPs body@.
data UnexpectedStatement where
  UnexpectedStatement
    :: Outputable (StmtLR GhcPs GhcPs body)
    => StmtLR GhcPs GhcPs body
    -> UnexpectedStatement

data DeclSort = ClassDeclSort | InstanceDeclSort

data NonStandardGuards where
  NonStandardGuards
    :: (Outputable body,
        Anno (Stmt GhcRn body) ~ SrcSpanAnnA)
    => [LStmtLR GhcRn GhcRn body]
    -> NonStandardGuards

data RuleLhsErrReason
  = UnboundVariable RdrName NotInScopeError
  | IllegalExpression

data HsigShapeMismatchReason =
  {-| HsigShapeSortMismatch is an error indicating that an item in the
    export list of a signature doesn't match the item of the same name in
    another signature when merging the two – one is a type while the other is a
    plain identifier.

    Test cases:
      none
  -}
  HsigShapeSortMismatch !AvailInfo !AvailInfo
  |
  {-| HsigShapeNotUnifiable is an error indicating that a name in the
    export list of a signature cannot be unified with a name of the same name in
    another signature when merging the two.

    Test cases:
      bkpfail20, bkpfail21
  -}
  HsigShapeNotUnifiable !Name !Name !Bool
  deriving (Generic)

data WrongThingSort
  = WrongThingType
  | WrongThingDataCon
  | WrongThingPatSyn
  | WrongThingConLike
  | WrongThingClass
  | WrongThingTyCon
  | WrongThingAxiom

data StageCheckReason
  = StageCheckInstance !InstanceWhat !PredType
  | StageCheckSplice !Name

data UninferrableTyVarCtx
  = UninfTyCtx_ClassContext [TcType]
  | UninfTyCtx_DataContext [TcType]
  | UninfTyCtx_ProvidedContext [TcType]
  | UninfTyCtx_TyFamRhs TcType
  | UninfTyCtx_TySynRhs TcType
  | UninfTyCtx_Sig TcType (LHsSigType GhcRn)

data PatSynInvalidRhsReason
  = PatSynNotInvertible !(Pat GhcRn)
  | PatSynUnboundVar !Name
  deriving (Generic)

data BadFieldAnnotationReason where
  {-| A lazy data type field annotation (~) was used without enabling the
    extension StrictData.

    Test cases:
    LazyFieldsDisabled
  -}
  LazyFieldsDisabled :: BadFieldAnnotationReason
  {-| An UNPACK pragma was applied to a field without strictness annotation (!).

    Test cases:
    T14761a, T7562
  -}
  UnpackWithoutStrictness :: BadFieldAnnotationReason
  {-| An UNPACK pragma was applied to an abstract type in an indefinite package
    in Backpack.

    Test cases:
    unpack_sums_5, T3966, T7050
  -}
  BackpackUnpackAbstractType :: BadFieldAnnotationReason
  deriving (Generic)

data SuperclassCycle =
  MkSuperclassCycle { cls :: Class, definite :: Bool, reasons :: [SuperclassCycleDetail] }

data SuperclassCycleDetail
  = SCD_HeadTyVar !PredType
  | SCD_HeadTyFam !PredType
  | SCD_Superclass !Class

data RoleValidationFailedReason
  = TyVarRoleMismatch !TyVar !Role
  | TyVarMissingInEnv !TyVar
  | BadCoercionRole !Coercion
  deriving (Generic)

data DisabledClassExtension where
  {-| MultiParamTypeClasses is required.

    Test cases:
    readFail037, TcNoNullaryTC
  -}
  MultiParamDisabled :: !Int -- ^ The arity
                     -> DisabledClassExtension
  {-| FunctionalDependencies is required.

    Test cases:
    readFail041
  -}
  FunDepsDisabled :: DisabledClassExtension
  {-| ConstrainedClassMethods is required.

    Test cases:
    mod39, tcfail150
  -}
  ConstrainedClassMethodsDisabled :: !Id
                                  -> !TcPredType
                                  -> DisabledClassExtension
  deriving (Generic)

data TyFamsDisabledReason
  = TyFamsDisabledFamily !Name
  | TyFamsDisabledInstance !TyCon
  deriving (Generic)

-- | Either `HsType p` or `HsSigType p`.
--
-- Used for reporting errors in `TcRnIllegalKind`.
data HsTypeOrSigType p
  = HsType    (HsType p)
  | HsSigType (HsSigType p)

instance OutputableBndrId p => Outputable (HsTypeOrSigType (GhcPass p)) where
  ppr (HsType ty) = ppr ty
  ppr (HsSigType sig_ty) = ppr sig_ty

-- | A wrapper around HsTyVarBndr.
-- Used for reporting errors in `TcRnUnusedQuantifiedTypeVar`.
data HsTyVarBndrExistentialFlag = forall flag. OutputableBndrFlag flag 'Renamed =>
  HsTyVarBndrExistentialFlag (HsTyVarBndr flag GhcRn)

instance Outputable HsTyVarBndrExistentialFlag where
  ppr (HsTyVarBndrExistentialFlag hsTyVarBndr) = ppr hsTyVarBndr

type TySynCycleTyCons =
  [Either TyCon (LTyClDecl GhcRn)]
