{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module GHC.Tc.Errors.Types (
  -- * Main types
    TcRnMessage(..)
  , TcRnMessageDetailed(..)
  , ErrInfo(..)
  , LevityCheckProvenance(..)
  , ShadowedNameProvenance(..)
  , RecordFieldPart(..)
  ) where

import GHC.Hs
import GHC.Tc.Types.Constraint
import GHC.Types.Error
import GHC.Types.Name (Name, OccName)
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Unit.Types (Module)
import GHC.Utils.Outputable
import GHC.Core.Type (Type, Var)
import GHC.Unit.State (UnitState)
import GHC.Types.Basic

import qualified Data.List.NonEmpty as NE
import           Data.Typeable

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
  -}
  TcRnUnsafeDueToPlugin :: TcRnMessage
  {-| TcRnModMissingRealSrcSpan is an error that occurrs when compiling a module that lacks
      an associated 'RealSrcSpan'.
  -}
  TcRnModMissingRealSrcSpan :: Module -> TcRnMessage


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
  TcRnSimplifierTooManyIterations :: !IntWithInf
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

