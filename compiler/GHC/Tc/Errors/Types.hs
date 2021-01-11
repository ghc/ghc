module GHC.Tc.Errors.Types (
  -- * Main type
    TcRnMessage(..)
  -- * Types for suggestions
  , HowInScope
  , ImportSuggestion(..)
  , NameSuggestions(..)
  , ExtensionSuggestion(..)
  , OutOfScopeSuggestions(..)
  -- * Constructing messages
  , mkTcRnWarn
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

-- | Creates a new 'ErrMsg' parameterised over the input 'Warning', attaching the
-- correct 'WarnReason' to it.
mkTcRnWarn :: WarnReason -> SrcSpan -> PrintUnqualified -> TcRnMessage -> MsgEnvelope TcRnMessage
mkTcRnWarn reason loc printer warn = makeIntoWarning reason (mkErr loc printer warn)

-- | An error which might arise during typechecking/renaming.
data TcRnMessage
  = TcRnUnknownMessage !DecoratedSDoc

  -- See 'mkErrDocAt' in 'GHC.Tc.Utils.Monad', where we need the 'UnitState'
  -- to render the 'Unit' properly. This is a type constructor to build an embellished
  -- 'Error' which can be pretty-printed with the fully qualified 'UnitState'.
  | TcRnMessageWithUnitState !UnitState !TcRnMessage

  -- Errors thrown in GHC.Tc.Errors
  | TcRnBadTelescope
      [RenderableTyVarBndr] -- telescope
      [TyCoVar] -- sorted tyvars (in a correct order)
      !SDoc      -- context
  | TcRnOutOfScope
      !RdrName -- name tried
      !OutOfScopeSuggestions -- similar name, import, etc suggestions
      !SDoc -- extra contents (see 'unboundNameX')
      !SDoc -- context lines
  | TcRnOutOfScopeHole
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
