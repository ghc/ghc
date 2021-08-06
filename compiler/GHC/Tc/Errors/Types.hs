{-# LANGUAGE GADTs #-}
module GHC.Tc.Errors.Types (
  -- * Main types
    TcRnMessage(..)
  , TcRnMessageDetailed(..)
  , ErrInfo(..)
  , LevityCheckProvenance(..)
  , ShadowedNameProvenance(..)
  , RecordFieldPart(..)
  , InjectivityErrReason(..)
  , HasKinds(..)
  , hasKinds
  , SuggestUndecidableInstances(..)
  , suggestUndecidableInstances
  ) where

import GHC.Prelude

import GHC.Hs
import {-# SOURCE #-} GHC.Tc.Types (TcIdSigInfo)
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Rank (Rank)
import GHC.Types.Error
import GHC.Types.Name (Name, OccName)
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Types.TyThing (TyThing)
import GHC.Unit.Types (Module)
import GHC.Utils.Outputable
import GHC.Core.Class (Class)
import GHC.Core.Coercion.Axiom (CoAxBranch)
import GHC.Core.FamInstEnv (FamInst)
import GHC.Core.InstEnv (ClsInst)
import GHC.Core.TyCon (TyCon, TyConFlavour)
import GHC.Core.Type (Kind, Type, Var)
import GHC.Unit.State (UnitState)
import GHC.Unit.Module.Name (ModuleName)
import GHC.Types.Basic
import GHC.Types.Var.Set (TyVarSet)

import qualified Data.List.NonEmpty as NE
import           Data.Typeable hiding (TyCon)

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
the final message would be composed only by two SDoc (which would then be bulletted like in
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
-- and this newtype captures it. See Note [Migrating TcM messages].
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

-- | An error which might arise during typechecking/renaming.
data TcRnMessage where
  {-| Simply wraps a generic 'Diagnostic' message @a@. It can be used by plugins
      to provide custom diagnostic messages originated during typechecking/renaming.
  -}
  TcRnUnknownMessage :: (Diagnostic a, Typeable a) => a -> TcRnMessage

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

  {-| A levity polymorphism check happening during TcRn.
  -}
  TcLevityPolyInType :: !Type
                     -> !LevityCheckProvenance
                     -> !ErrInfo -- Extra info accumulated in the TcM monad
                     -> TcRnMessage

  {-| TcRnImplicitLift is a warning (controlled with -Wimplicit-lift) that occurs when
      a Template Haskell quote implicitly uses 'lift'.

     Example:
       warning1 :: Lift t => t -> Q Exp
       warning1 x = [| x |]

     Test cases: th/T17804
  -}
  TcRnImplicitLift :: Outputable var => var -> !ErrInfo -> TcRnMessage
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
  {-| TcRnDodgyImports is a warning (controlled with -Wdodgy-imports) that occurs when
      a datatype 'T' is imported with all constructors, i.e. 'T(..)', but has been exported
      abstractly, i.e. 'T'.

     Test cases: rename/should_compile/T7167
  -}
  TcRnDodgyImports :: RdrName -> TcRnMessage
  {-| TcRnDodgyExports is a warning (controlled by -Wdodgy-exports) that occurs when a datatype
      'T' is exported with all constructors, i.e. 'T(..)', but is it just a type synonym or a
      type/data family.

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
  TcRnDodgyExports :: Name -> TcRnMessage
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
  {-| TcRnModMissingRealSrcSpan is an error that occurrs when compiling a module that lacks
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

  {-| TcRnDuplicateWarningDecls is an error that occurs whenever
      a warning is declared twice.

      Examples(s):
        None.

     Test cases:
        None.
  -}
  TcRnDuplicateWarningDecls :: !(LocatedN RdrName) -> !RdrName -> TcRnMessage

  {-| TcRnDuplicateWarningDecls is an error that occurs whenever
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

  {-| TcRnDuplicateFieldName is an error that occurs whenever
      there are duplicate field names in a record.

      Examples(s): None.

     Test cases: None.
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

  {-| TcRnIllegalWildcardsInConstructor is an error that occurs whenever
      the record wildcards '..' are used inside a constructor without labeled fields.

      Examples(s): None

     Test cases: None
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

  {-| TcRnArrowIfThenElsePredDependsOnResultTy is an error that occurs when the
      predicate type of an ifThenElse expression in arrow notation depends on
      the type of the result.

      Example(s): None

     Test cases: None
  -}
  TcRnArrowIfThenElsePredDependsOnResultTy :: TcRnMessage

  {-| TcRnArrowCommandExpected is an error that occurs if a non-arrow command
      is used where an arrow command is expected.

      Example(s): None

     Test cases: None
  -}
  TcRnArrowCommandExpected :: HsCmd GhcRn -> TcRnMessage

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
    :: Name -- ^ type variable being quantified
    -> Name -- ^ function name
    -> LHsSigWcType GhcRn -> TcRnMessage

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

  {-| TcRnUnboxedTupleTypeFuncArg is an error that occurs whenever an unboxed tuple type
      is specified as a function argument.

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
  TcRnUnboxedTupleTypeFuncArg :: !Type -> TcRnMessage

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
  TcRnVDQInTermType :: !Type -> TcRnMessage

  {-| TcRnIllegalEqualConstraints is an error that occurs whenever an illegal equational
      constraint is specified.

      Examples(s):
        blah :: (forall a. a b ~ a c) => b -> c
        blah = undefined

      Test cases: typecheck/should_fail/T17563
  -}
  TcRnIllegalEqualConstraints :: !Type -> TcRnMessage

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
  TcRnIllegalClassInst :: !TyConFlavour -> TcRnMessage

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

  {-| TcRnOrphanInstance is a warning (controlled by -Wwarn-orphans)
      that arises when a typeclass instance is an \"orphan\", i.e. if it appears
      in a module in which neither the class nor the type being instanced are
      declared in the same module.

      Examples(s): None

      Test cases: warnings/should_compile/T9178
                  typecheck/should_compile/T4912
  -}
  TcRnOrphanInstance :: ClsInst -> TcRnMessage

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
  TcRnDuplicateExport :: GreName -> IE GhcPs -> IE GhcPs -> TcRnMessage

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
  TcRnExportedParentChildMismatch :: Name -> TyThing -> GreName -> [Name] -> TcRnMessage

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
    :: OccName -- ^ Occurrence name shared by both exports
    -> GreName -- ^ Name of first export
    -> GlobalRdrElt -- ^ Provenance for definition site of first export
    -> IE GhcPs -- ^ Export decl of first export
    -> GreName -- ^ Name of second export
    -> GlobalRdrElt -- ^ Provenance for definition site of second export
    -> IE GhcPs -- ^ Export decl of second export
    -> TcRnMessage

-- | Which parts of a record field are affected by a particular error or warning.
data RecordFieldPart
  = RecordFieldConstructor !Name
  | RecordFieldPattern !Name
  | RecordFieldUpdate

-- | Where a shadowed name comes from
data ShadowedNameProvenance
  = ShadowedNameProvenanceLocal !SrcLoc
    -- ^ The shadowed name is local to the module
  | ShadowedNameProvenanceGlobal [GlobalRdrElt]
    -- ^ The shadowed name is global, typically imported from elsewhere.

-- | Where the levity checking for the input type originated
data LevityCheckProvenance
  = LevityCheckInVarType
  | LevityCheckInBinder !Var
  | LevityCheckInWildcardPattern
  | LevityCheckInUnboxedTuplePattern !(Pat GhcTc)
  | LevityCheckPatSynSig
  | LevityCheckCmdStmt
  | LevityCheckMkCmdEnv !Var
  | LevityCheckDoCmd !(HsCmd GhcTc)
  | LevityCheckDesugaringCmd !(LHsCmd GhcTc)
  | LevityCheckInCmd !(LHsCmd GhcTc)
  | LevityCheckInFunUse !(LHsExpr GhcTc)
  | LevityCheckInValidDataCon
  | LevityCheckInValidClass

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
