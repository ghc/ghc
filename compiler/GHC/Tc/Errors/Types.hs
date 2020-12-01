module GHC.Tc.Errors.Types (
  -- * Error and warning types
    Error(..)
  , Warning(..)
  , DsError
  , DsWarning
  -- * Types for suggestions
  , HowInScope
  , ImportSuggestion(..)
  , NameSuggestions(..)
  , ExtensionSuggestion(..)
  , OutOfScopeSuggestions(..)
  -- * Constructing warnings
  , tcRnWarnRaw
  , mkTcRnWarn
  , demoteTcRnError
  -- * Constructing suggestions
  , noOutOfScopeSuggestions
  ) where

import GHC.Driver.Flags
import GHC.Prelude
import GHC.Tc.Types.Origin ( RenderableTyVarBndr )
import GHC.Tc.Utils.TcType
import GHC.Types.Error
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Types.Var
import GHC.Unit.Module.Name
import GHC.Unit.State ( UnitState )
import GHC.Unit.Types
import GHC.Utils.Outputable

-- FIXME(adinapoli) Untangle this.
type DsError   = Error
type DsWarning = Warning

-- | A warning which might arise during typechecking/renaming.
data Warning
  = WarnTcRnRaw !ErrDoc
  | WarnTcRnDemotedErr !Error
  -- In the future we could add more specific warnings, like:
  -- | WarnTypedHoles
  -- | WarnPartialTypeSignatures
  -- | WarnDeferredOutOfScopeVariables
  -- | WarnInaccessibleCode
  -- | WarnRedundantConstraints

-- | Creates a new 'ErrMsg' parameterised over the input 'Warning', attaching the
-- correct 'WarnReason' to it.
mkTcRnWarn :: WarnReason -> SrcSpan -> PrintUnqualified -> Warning -> ErrMsg Warning
mkTcRnWarn reason loc printer warn = makeIntoWarning reason (mkErr loc printer warn)

tcRnWarnRaw :: MsgDoc -> MsgDoc -> Warning
tcRnWarnRaw msg extra_info = WarnTcRnRaw (errDoc [msg] [] [extra_info])

-- | Demotes a typecheck-rename 'Error' into a 'Warning'.
demoteTcRnError :: WarnReason -> ErrMsg Error -> ErrMsg Warning
demoteTcRnError reason = makeIntoWarning reason . fmap WarnTcRnDemotedErr

-- | An error which might arise during typechecking/renaming.
data Error
  = ErrTcRnRaw !ErrDoc

  -- See 'mkErrDocAt' in 'GHC.Tc.Utils.Monad', where we need the 'UnitState'
  -- to render the 'Unit' properly. This is a type constructor to build an embellished
  -- 'Error' which can be pretty-printed with the fully qualified 'UnitState'.
  | ErrorTcRnWithUnitState !UnitState !Error

  -- Errors thrown in GHC.Tc.Errors
  | ErrBadTelescope
      [RenderableTyVarBndr] -- telescope
      [TyCoVar] -- sorted tyvars (in a correct order)
      !SDoc      -- context
  | ErrOutOfScope
      !RdrName -- name tried
      !OutOfScopeSuggestions -- similar name, import, etc suggestions
      !SDoc -- extra contents (see 'unboundNameX')
      !SDoc -- context lines
  | ErrOutOfScopeHole
      !OccName -- out of scope name
      !TcType  -- type of the hole
      !OutOfScopeSuggestions -- similar name, import, etc suggestions

type HowInScope = Either SrcSpan ImpDeclSpec
     -- Left loc    =>  locally bound at loc
     -- Right ispec =>  imported as specified by ispec

data OutOfScopeSuggestions = OutOfScopeSuggestions
  { oosSimilarNames :: Maybe NameSuggestions
  , oosImports      :: Maybe ImportSuggestion
  , oosExtensions   :: Maybe ExtensionSuggestion
  }

noOutOfScopeSuggestions :: OutOfScopeSuggestions
noOutOfScopeSuggestions = OutOfScopeSuggestions Nothing Nothing Nothing

newtype NameSuggestions = NameSuggestions [(RdrName, HowInScope)]

data ImportSuggestion
  = SuggestNoModuleImported !ModuleName
  | SuggestModulesDoNotExport [Module] !OccName
  | SuggestAddNameToImportLists !OccName [(Module, SrcSpan)]
  | SuggestRemoveNameFromHidingLists !OccName [(Module, SrcSpan)]

data ExtensionSuggestion = SuggestRecursiveDo
