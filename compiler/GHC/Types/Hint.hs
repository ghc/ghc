{-# LANGUAGE ExistentialQuantification #-}

module GHC.Types.Hint (
    GhcHint(..)
  , AvailableBindings(..)
  , InstantiationSuggestion(..)
  , LanguageExtensionHint(..)
  , ImportSuggestion(..)
  , HowInScope(..)
  , SimilarName(..)
  , StarIsType(..)
  , UntickedPromotedThing(..)
  , AssumedDerivingStrategy(..)
  , pprUntickedConstructor, isBareSymbol
  , suggestExtension
  , suggestExtensionWithInfo
  , suggestExtensions
  , suggestExtensionsWithInfo
  , suggestAnyExtension
  , suggestAnyExtensionWithInfo
  , useExtensionInOrderTo
  , noStarIsTypeHints
  ) where

import Language.Haskell.Syntax.Expr (LHsExpr)
import Language.Haskell.Syntax (LPat, LIdP, LHsSigType, LHsSigWcType)

import GHC.Prelude

import qualified Data.List.NonEmpty as NE

import qualified GHC.LanguageExtensions as LangExt
import GHC.Unit.Module (ModuleName, Module)
import GHC.Unit.Module.Imported (ImportedModsVal)
import GHC.Hs.Extension (GhcTc, GhcRn)
import GHC.Core.Coercion
import GHC.Core.FamInstEnv (FamFlavor)
import GHC.Core.TyCon (TyCon)
import GHC.Core.Type (Type)
import GHC.Types.Fixity (LexicalFixity(..))
import GHC.Types.Name (Name, NameSpace, OccName (occNameFS), isSymOcc, nameOccName)
import GHC.Types.Name.Reader (RdrName (Unqual), ImpDeclSpec)
import GHC.Types.SrcLoc (SrcSpan)
import GHC.Types.Basic (Activation, RuleName)
import GHC.Parser.Errors.Basic
import GHC.Utils.Outputable
import GHC.Data.FastString (fsLit, FastString)

import Data.Typeable ( Typeable )
import Data.Map.Strict (Map)

-- | The bindings we have available in scope when
-- suggesting an explicit type signature.
data AvailableBindings
  = NamedBindings  (NE.NonEmpty Name)
  | UnnamedBinding
  -- ^ An unknown binding (i.e. too complicated to turn into a 'Name')


data LanguageExtensionHint
  = -- | Suggest to enable the input extension. This is the hint that
    -- GHC emits if this is not a \"known\" fix, i.e. this is GHC giving
    -- its best guess on what extension might be necessary to make a
    -- certain program compile. For example, GHC might suggests to
    -- enable 'BlockArguments' when the user simply formatted incorrectly
    -- the input program, so GHC here is trying to be as helpful as
    -- possible.
    -- If the input 'SDoc' is not empty, it will contain some extra
    -- information about the why the extension is required, but
    -- it's totally irrelevant/redundant for IDEs and other tools.
     SuggestSingleExtension !SDoc !LangExt.Extension
    -- | Suggest to enable the input extensions. The list
    -- is to be intended as /disjunctive/ i.e. the user is
    -- suggested to enable /any/ of the extensions listed. If
    -- the input 'SDoc' is not empty, it will contain some extra
    -- information about the why the extensions are required, but
    -- it's totally irrelevant/redundant for IDEs and other tools.
  | SuggestAnyExtension !SDoc [LangExt.Extension]
    -- | Suggest to enable the input extensions. The list
    -- is to be intended as /conjunctive/ i.e. the user is
    -- suggested to enable /all/ the extensions listed. If
    -- the input 'SDoc' is not empty, it will contain some extra
    -- information about the why the extensions are required, but
    -- it's totally irrelevant/redundant for IDEs and other tools.
  | SuggestExtensions !SDoc [LangExt.Extension]
    -- | Suggest to enable the input extension in order to fix
    -- a certain problem. This is the suggestion that GHC emits when
    -- is more-or-less clear \"what's going on\". For example, if
    -- both 'DeriveAnyClass' and 'GeneralizedNewtypeDeriving' are
    -- turned on, the right thing to do is to enabled 'DerivingStrategies',
    -- so in contrast to 'SuggestSingleExtension' GHC will be a bit more
    -- \"imperative\" (i.e. \"Use X Y Z in order to ... \").
    -- If the input 'SDoc' is not empty, it will contain some extra
    -- information about the why the extensions are required, but
    -- it's totally irrelevant/redundant for IDEs and other tools.
  | SuggestExtensionInOrderTo !SDoc !LangExt.Extension

-- | Suggests a single extension without extra user info.
suggestExtension :: LangExt.Extension -> GhcHint
suggestExtension ext = SuggestExtension (SuggestSingleExtension empty ext)

-- | Like 'suggestExtension' but allows supplying extra info for the user.
suggestExtensionWithInfo :: SDoc -> LangExt.Extension -> GhcHint
suggestExtensionWithInfo extraInfo ext = SuggestExtension (SuggestSingleExtension extraInfo ext)

-- | Suggests to enable /every/ extension in the list.
suggestExtensions :: [LangExt.Extension] -> GhcHint
suggestExtensions exts = SuggestExtension (SuggestExtensions empty exts)

-- | Like 'suggestExtensions' but allows supplying extra info for the user.
suggestExtensionsWithInfo :: SDoc -> [LangExt.Extension] -> GhcHint
suggestExtensionsWithInfo extraInfo exts = SuggestExtension (SuggestExtensions extraInfo exts)

-- | Suggests to enable /any/ extension in the list.
suggestAnyExtension :: [LangExt.Extension] -> GhcHint
suggestAnyExtension exts = SuggestExtension (SuggestAnyExtension empty exts)

-- | Like 'suggestAnyExtension' but allows supplying extra info for the user.
suggestAnyExtensionWithInfo :: SDoc -> [LangExt.Extension] -> GhcHint
suggestAnyExtensionWithInfo extraInfo exts = SuggestExtension (SuggestAnyExtension extraInfo exts)

useExtensionInOrderTo :: SDoc -> LangExt.Extension -> GhcHint
useExtensionInOrderTo extraInfo ext = SuggestExtension (SuggestExtensionInOrderTo extraInfo ext)

-- | A type for hints emitted by GHC.
-- A /hint/ suggests a possible way to deal with a particular warning or error.
data GhcHint
  =
    {-| An \"unknown\" hint. This type constructor allows arbitrary
    -- hints to be embedded. The typical use case would be GHC plugins
    -- willing to emit hints alongside their custom diagnostics.
    -}
    forall a. (Outputable a, Typeable a) => UnknownHint a
    {-| Suggests adding a particular language extension. GHC will do its best trying
        to guess when the user is using the syntax of a particular language extension
        without having the relevant extension enabled.

        Example: If the user uses the keyword \"mdo\" (and we are in a monadic block), but
        the relevant extension is not enabled, GHC will emit a 'SuggestExtension RecursiveDo'.

        Test case(s): parser/should_fail/T12429, parser/should_fail/T8501c,
                      parser/should_fail/T18251e, ... (and many more)

    -}
  | SuggestExtension !LanguageExtensionHint
    {-| Suggests possible corrections of a misspelled pragma. Its argument
        represents all applicable suggestions.

        Example: {-# LNGUAGE BangPatterns #-}

        Test case(s): parser/should_compile/T21589
    -}
  | SuggestCorrectPragmaName ![String]
    {-| Suggests that a monadic code block is probably missing a \"do\" keyword.

        Example:
            main =
              putStrLn "hello"
              putStrLn "world"

        Test case(s): parser/should_fail/T8501a, parser/should_fail/readFail007,
                      parser/should_fail/InfixAppPatErr, parser/should_fail/T984
    -}
  | SuggestMissingDo
    {-| Suggests that a \"let\" expression is needed in a \"do\" block.

       Test cases: None (that explicitly test this particular hint is emitted).
    -}
  | SuggestLetInDo
    {-| Suggests to add an \".hsig\" signature file to the Cabal manifest.

      Triggered by: 'GHC.Driver.Errors.Types.DriverUnexpectedSignature', if Cabal
                    is being used.

      Example: See comment of 'DriverUnexpectedSignature'.

      Test case(s): driver/T12955

    -}
  | SuggestAddSignatureCabalFile !ModuleName
    {-| Suggests to explicitly list the instantiations for the signatures in
        the GHC invocation command.

      Triggered by: 'GHC.Driver.Errors.Types.DriverUnexpectedSignature', if Cabal
                    is /not/ being used.

      Example: See comment of 'DriverUnexpectedSignature'.

      Test case(s): driver/T12955
    -}
  | SuggestSignatureInstantiations !ModuleName [InstantiationSuggestion]
    {-| Suggests to use spaces instead of tabs.

        Triggered by: 'GHC.Parser.Errors.Types.PsWarnTab'.

        Examples: None
        Test Case(s): None
    -}
  | SuggestUseSpaces
    {-| Suggests adding a whitespace after the given symbol.

        Examples: None
        Test Case(s): parser/should_compile/T18834a.hs
    -}
  | SuggestUseWhitespaceAfter !OperatorWhitespaceSymbol
    {-| Suggests adding a whitespace around the given operator symbol,
        as it might be repurposed as special syntax by a future language extension.
        The second parameter is how such operator occurred, if in a prefix, suffix
        or tight infix position.

        Triggered by: 'GHC.Parser.Errors.Types.PsWarnOperatorWhitespace'.

        Example:
          h a b = a+b -- not OK, no spaces around '+'.

        Test Case(s): parser/should_compile/T18834b.hs
    -}
  | SuggestUseWhitespaceAround !String !OperatorWhitespaceOccurrence
    {-| Suggests wrapping an expression in parentheses

        Examples: None
        Test Case(s): None
    -}
  | SuggestParentheses
    {-| Suggests to increase the -fmax-pmcheck-models limit for the pattern match checker.

      Triggered by: 'GHC.HsToCore.Errors.Types.DsMaxPmCheckModelsReached'

      Test case(s): pmcheck/should_compile/TooManyDeltas
                    pmcheck/should_compile/TooManyDeltas
                    pmcheck/should_compile/T11822
    -}
  | SuggestIncreaseMaxPmCheckModels
    {-| Suggests adding a type signature, typically to resolve ambiguity or help GHC inferring types.

    -}
  | SuggestAddTypeSignatures AvailableBindings
    {-| Suggests to explicitly discard the result of a monadic action by binding the result to
        the '_' wilcard.

        Example:
           main = do
             _ <- getCurrentTime

    -}
  | SuggestBindToWildcard !(LHsExpr GhcTc)

  | SuggestAddInlineOrNoInlinePragma !Var !Activation

  | SuggestAddPhaseToCompetingRule !RuleName
    {-| Suggests adding an identifier to the export list of a signature.
    -}
  | SuggestAddToHSigExportList !Name !(Maybe Module)
    {-| Suggests increasing the limit for the number of iterations in the simplifier.
    -}
  | SuggestIncreaseSimplifierIterations
    {-| Suggests to explicitly import 'Type' from the 'Data.Kind' module, because
        using "*" to mean 'Data.Kind.Type' relies on the StarIsType extension, which
        will become deprecated in the future.

        Triggered by: 'GHC.Parser.Errors.Types.PsWarnStarIsType'
        Example: None
        Test case(s): wcompat-warnings/WCompatWarningsOn.hs

    -}
  | SuggestUseTypeFromDataKind (Maybe RdrName)

    {-| Suggests placing the 'qualified' keyword /after/ the module name.

        Triggered by: 'GHC.Parser.Errors.Types.PsWarnImportPreQualified'
        Example: None
        Test case(s): module/mod184.hs

    -}
  | SuggestQualifiedAfterModuleName

    {-| Suggests using TemplateHaskell quotation syntax.

        Triggered by: 'GHC.Parser.Errors.Types.PsErrEmptyDoubleQuotes' only if TemplateHaskell
                      is enabled.
        Example: None
        Test case(s): parser/should_fail/T13450TH.hs

    -}
  | SuggestThQuotationSyntax

    {-| Suggests alternative roles in case we found an illegal one.

        Triggered by: 'GHC.Parser.Errors.Types.PsErrIllegalRoleName'
        Example: None
        Test case(s): roles/should_fail/Roles7.hs

    -}
  | SuggestRoles [Role]

    {-| Suggests qualifying the '*' operator in modules where StarIsType is enabled.

        Triggered by: 'GHC.Parser.Errors.Types.PsWarnStarBinder'
        Test case(s): warnings/should_compile/StarBinder.hs
    -}
  | SuggestQualifyStarOperator

    {-| Suggests that for a type signature 'M.x :: ...' the qualifier should be omitted
        in order to be accepted by GHC.

        Triggered by: 'GHC.Parser.Errors.Types.PsErrInvalidTypeSignature'
        Test case(s): module/mod98
    -}
  | SuggestTypeSignatureRemoveQualifier

    {-| Suggests to move an orphan instance (for a typeclass or a type or data
        family), or to newtype-wrap it.

        Triggered by: 'GHC.Tc.Errors.Types.TcRnOrphanInstance'
        Test cases(s): warnings/should_compile/T9178
                       typecheck/should_compile/T4912
                       indexed-types/should_compile/T22717_fam_orph
    -}
  | SuggestFixOrphanInst
    { isFamilyInstance :: Maybe FamFlavor }
      -- ^ Whether this is a family instance (of the given 'FamFlavor'),
      -- or a class instance ('Nothing').

    {-| Suggests to use a standalone deriving declaration when GHC
        can't derive a typeclass instance in a trivial way.

        Triggered by: 'GHC.Tc.Errors.Types.DerivBadErrConstructor'
        Test cases(s): typecheck/should_fail/tcfail086
    -}
  | SuggestAddStandaloneDerivation

    {-| Suggests to add a standalone kind signature when GHC
        can't perform kind inference.

        Triggered by: 'GHC.Tc.Errors.Types.TcRnInvisBndrWithoutSig'
        Test case(s): typecheck/should_fail/T22560_fail_d
    -}
  | SuggestAddStandaloneKindSignature Name

    {-| Suggests the user to fill in the wildcard constraint to
        disambiguate which constraint that is.

        Example:
          deriving instance _ => Eq (Foo f a)

        Triggered by: 'GHC.Tc.Errors.Types.DerivBadErrConstructor'
        Test cases(s): partial-sigs/should_fail/T13324_fail2
    -}
  | SuggestFillInWildcardConstraint

    {-| Suggests to use the appropriate Template Haskell tick:
        a single tick for a term-level 'NameSpace', or a double tick
        for a type-level 'NameSpace'.

        Triggered by: 'GHC.Tc.Errors.Types.TcRnIncorrectNameSpace'.
    -}
  | SuggestAppropriateTHTick NameSpace

  {-| Suggests enabling -ddump-splices to help debug an issue
      when a 'Name' is not in scope or is used in multiple
      different namespaces (e.g. both as a data constructor
      and a type constructor).

      Concomitant with 'NoExactName' or 'SameName' errors,
      see e.g. "GHC.Rename.Env.lookupExactOcc_either".
      Test cases: T5971, T7241, T13937.
   -}
  | SuggestDumpSlices

  {-| Suggests adding a tick to refer to something which has been
      promoted to the type level, e.g. a data constructor.

      Test cases: T9778, T19984.
  -}
  | SuggestAddTick UntickedPromotedThing

  {-| Something is split off from its corresponding declaration.
      For example, a datatype is given a role declaration
      in a different module.

      Test cases: T495, T8485, T2713, T5533.
   -}
  | SuggestMoveToDeclarationSite
      -- TODO: remove the SDoc argument.
      SDoc -- ^ fixity declaration, role annotation, type signature, ...
      RdrName -- ^ the 'RdrName' for the declaration site

  {-| Suggest a similar name that the user might have meant,
      e.g. suggest 'traverse' when the user has written @travrese@.

      Test case: mod73.
  -}
  | SuggestSimilarNames RdrName (NE.NonEmpty SimilarName)

  {-| Remind the user that the field selector has been suppressed
      because of -XNoFieldSelectors.

      Test cases: NFSSuppressed, records-nofieldselectors.
  -}
  | RemindFieldSelectorSuppressed
      { suppressed_selector :: RdrName
      , suppressed_parents  :: [Name] }

  {-| Suggest importing from a module, removing a @hiding@ clause,
      or explain to the user that we couldn't find a module
      with the given 'ModuleName'.

      Test cases: mod28, mod36, mod87, mod114, ...
  -}
  | ImportSuggestion OccName ImportSuggestion

  {-| Found a pragma in the body of a module, suggest placing it in the header.
  -}
  | SuggestPlacePragmaInHeader
    {-| Suggest using pattern matching syntax for a non-bidirectional pattern synonym

        Test cases: patsyn/should_fail/record-exquant
                    typecheck/should_fail/T3176
    -}
  | SuggestPatternMatchingSyntax
    {-| Suggest tips for making a definition visible for the purpose of writing
        a SPECIALISE pragma for it in a different module.

        Test cases: none
    -}
  | SuggestSpecialiseVisibilityHints Name

    {-| Suggest renaming implicitly quantified type variable in case it
        captures a term's name.
    -}
  | SuggestRenameTypeVariable

  | SuggestExplicitBidiPatSyn Name (LPat GhcRn) [LIdP GhcRn]

    {-| Suggest enabling one of the SafeHaskell modes Safe, Unsafe or
        Trustworthy.
    -}
  | SuggestSafeHaskell

    {-| Suggest removing a record wildcard from a pattern when it doesn't
        bind anything useful.
    -}
  | SuggestRemoveRecordWildcard
    {-| Suggest moving a method implementation to a different instance to its
      superclass that defines the canonical version of the method.
    -}
  | SuggestMoveNonCanonicalDefinition
    Name -- ^ move the implementation from this method
    Name -- ^ ... to this method
    String -- ^ Documentation URL

    {-| Suggest to increase the solver maximum reduction depth -}
  | SuggestIncreaseReductionDepth

    {-| Suggest removing a method implementation when a superclass defines the
      canonical version of that method.
    -}
  | SuggestRemoveNonCanonicalDefinition
    Name -- ^ method with non-canonical implementation
    Name -- ^ possible other method to use as the RHS instead
    String -- ^ Documentation URL
  {-| Suggest eta-reducing a type synonym used in the implementation
      of abstract data. -}
  | SuggestEtaReduceAbsDataTySyn TyCon
  {-| Remind the user that there is no field of a type and name in the record,
      constructors are in the usual order $x$, $r$, $a$ -}
  | RemindRecordMissingField FastString Type Type
  {-| Suggest binding the type variable on the LHS of the type declaration
  -}
  | SuggestBindTyVarOnLhs RdrName

  {-| Suggest using an anonymous wildcard instead of a named wildcard -}
  | SuggestAnonymousWildcard

  {-| Suggest explicitly quantifying a type variable instead of relying on implicit quantification -}
  | SuggestExplicitQuantification RdrName

  {-| Suggest binding explicitly; e.g   data T @k (a :: F k) = .... -}
  | SuggestBindTyVarExplicitly Name

  {-| Suggest a default declaration; e.g @default Cls (Ty1, Ty2)@ -}
  | SuggestDefaultDeclaration TyCon [Type]

  {-| Suggest using explicit deriving strategies for a deriving clause.

      Triggered by: 'GHC.Tc.Errors.Types.TcRnNoDerivingClauseStrategySpecified'.

      See comment of 'TcRnNoDerivingClauseStrategySpecified' for context.
  -}
  | SuggestExplicitDerivingClauseStrategies
    (Map AssumedDerivingStrategy [LHsSigType GhcRn])
    -- ^ Those deriving clauses that we assumed a particular strategy for.

  {-| Suggest using an explicit deriving strategy for a standalone deriving instance.

      Triggered by: 'GHC.Tc.Errors.Types.TcRnNoStandaloneDerivingStrategySpecified'.

      See comment of 'TcRnNoStandaloneDerivingStrategySpecified' for context.
  -}
  | SuggestExplicitStandaloneDerivingStrategy
    AssumedDerivingStrategy -- ^ The deriving strategy we assumed
    (LHsSigWcType GhcRn) -- ^ The instance signature (e.g 'Show a => Show (T a)')

  {-| Suggest add parens to pattern `e -> p :: t` -}
  | SuggestParenthesizePatternRHS

-- | The deriving strategy that was assumed when not explicitly listed in the
--   source. This is used solely by the missing-deriving-strategies warning.
--   There's no `Via` case because we never assume that.
data AssumedDerivingStrategy
  = AssumedStockStrategy
  | AssumedAnyclassStrategy
  | AssumedNewtypeStrategy
  deriving (Eq, Ord)

instance Outputable AssumedDerivingStrategy where
  ppr AssumedStockStrategy = text "stock"
  ppr AssumedAnyclassStrategy = text "anyclass"
  ppr AssumedNewtypeStrategy = text "newtype"

-- | An 'InstantiationSuggestion' for a '.hsig' file. This is generated
-- by GHC in case of a 'DriverUnexpectedSignature' and suggests a way
-- to instantiate a particular signature, where the first argument is
-- the signature name and the second is the module where the signature
-- was defined.
-- Example:
--
-- src/MyStr.hsig:2:11: error:
--     Unexpected signature: ‘MyStr’
--     (Try passing -instantiated-with="MyStr=<MyStr>"
--      replacing <MyStr> as necessary.)
data InstantiationSuggestion = InstantiationSuggestion !ModuleName !Module

-- | Suggest how to fix an import.
data ImportSuggestion
  -- | Some module exports what we want, but we aren't explicitly importing it.
  = CouldImportFrom (NE.NonEmpty (Module, ImportedModsVal))
  -- | Some module exports what we want, but we are explicitly hiding it.
  | CouldUnhideFrom (NE.NonEmpty (Module, ImportedModsVal))
  -- | The module exports what we want, but it isn't a type.
  | CouldRemoveTypeKeyword ModuleName
  -- | The module exports what we want, but it's a type and we have @ExplicitNamespaces@ on.
  | CouldAddTypeKeyword ModuleName
  -- | Suggest importing a data constructor to bring it into scope
  | ImportDataCon
      -- | Where to suggest importing the 'DataCon' from.
      --
      -- The 'Bool' tracks whether to suggest using an import of the form
      -- @import (pattern Foo)@, depending on whether @-XPatternSynonyms@
      -- was enabled.
      { ies_suggest_import_from :: Maybe (ModuleName, Bool)
        -- | The 'OccName' of the parent of the data constructor.
      , ies_parent :: OccName }

-- | Explain how something is in scope.
data HowInScope
  -- | It was locally bound at this particular source location.
  = LocallyBoundAt SrcSpan
  -- | It was imported by this particular import declaration.
  | ImportedBy ImpDeclSpec

data SimilarName
  = SimilarName Name
  | SimilarRdrName RdrName (Maybe HowInScope)

-- | Something is promoted to the type-level without a promotion tick.
data UntickedPromotedThing
  = UntickedConstructor LexicalFixity Name
  | UntickedExplicitList

pprUntickedConstructor :: LexicalFixity -> Name -> SDoc
pprUntickedConstructor fixity nm =
  case fixity of
    Prefix -> pprPrefixVar is_op ppr_nm -- e.g. (:) and '(:)
    Infix  -> pprInfixVar  is_op ppr_nm -- e.g. `Con` and '`Con`
  where
    ppr_nm = ppr nm
    is_op = isSymOcc (nameOccName nm)

-- | Whether a constructor name is printed out as a bare symbol, e.g. @:@.
--
-- True for symbolic names in infix position.
--
-- Used for pretty-printing.
isBareSymbol :: LexicalFixity -> Name -> Bool
isBareSymbol fixity nm
  | isSymOcc (nameOccName nm)
  , Infix <- fixity
  = True
  | otherwise
  = False

--------------------------------------------------------------------------------

-- | Whether '*' is a synonym for 'Data.Kind.Type'.
data StarIsType
  = StarIsNotType
  | StarIsType

-- | Display info about the treatment of '*' under NoStarIsType.
--
-- With StarIsType, three properties of '*' hold:
--
--   (a) it is not an infix operator
--   (b) it is always in scope
--   (c) it is a synonym for Data.Kind.Type
--
-- However, the user might not know that they are working on a module with
-- NoStarIsType and write code that still assumes (a), (b), and (c), which
-- actually do not hold in that module.
--
-- Violation of (a) shows up in the parser. For instance, in the following
-- examples, we have '*' not applied to enough arguments:
--
--   data A :: *
--   data F :: * -> *
--
-- Violation of (b) or (c) show up in the renamer and the typechecker
-- respectively. For instance:
--
--   type K = Either * Bool
--
-- This will parse differently depending on whether StarIsType is enabled,
-- but it will parse nonetheless. With NoStarIsType it is parsed as a type
-- operator, thus we have ((*) Either Bool). Now there are two cases to
-- consider:
--
--   1. There is no definition of (*) in scope. In this case the renamer will
--      fail to look it up. This is a violation of assumption (b).
--
--   2. There is a definition of the (*) type operator in scope (for example
--      coming from GHC.TypeNats). In this case the user will get a kind
--      mismatch error. This is a violation of assumption (c).
--
-- The user might unknowingly be working on a module with NoStarIsType
-- or use '*' as 'Data.Kind.Type' out of habit. So it is important to give a
-- hint whenever an assumption about '*' is violated. Unfortunately, it is
-- somewhat difficult to deal with (c), so we limit ourselves to (a) and (b).
--
-- 'noStarIsTypeHints' returns appropriate hints to the user depending on the
-- extensions enabled in the module and the name that triggered the error.
-- That is, if we have NoStarIsType and the error is related to '*' or its
-- Unicode variant, we will suggest using 'Data.Kind.Type'; otherwise we won't
-- suggest anything.
noStarIsTypeHints :: StarIsType -> RdrName -> [GhcHint]
noStarIsTypeHints is_star_type rdr_name
  -- One might ask: if can use `sdocOption sdocStarIsType` here, why bother to
  -- take star_is_type as input? Why not refactor?
  --
  -- The reason is that `sdocOption sdocStarIsType` would indicate that
  -- StarIsType is enabled in the module that tries to load the problematic
  -- definition, not in the module that is being loaded.
  --
  -- So if we have 'data T :: *' in a module with NoStarIsType, then the hint
  -- must be displayed even if we load this definition from a module (or GHCi)
  -- with StarIsType enabled!
  --
  | isUnqualStar
  , StarIsNotType <- is_star_type
  = [SuggestUseTypeFromDataKind (Just rdr_name)]
  | otherwise
  = []
  where
    -- Does rdr_name look like the user might have meant the '*' kind by it?
    -- We focus on unqualified stars specifically, because qualified stars are
    -- treated as type operators even under StarIsType.
    isUnqualStar
      | Unqual occName <- rdr_name
      = let fs = occNameFS occName
        in fs == fsLit "*" || fs == fsLit "★"
      | otherwise = False
