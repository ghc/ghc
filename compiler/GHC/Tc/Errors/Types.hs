module GHC.Tc.Errors.Types (
  -- * Main type
    TcRnMessage(..)
  , TcRnDsMessage(..)
  , mkTcRnDsMessage
  -- * Types for suggestions
  , HowInScope
  , ImportSuggestion(..)
  , NameSuggestions(..)
  , ExtensionSuggestion(..)
  , OutOfScopeSuggestions(..)
  -- * Constructing suggestions
  , noOutOfScopeSuggestions
  ) where

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
import Data.List.NonEmpty (NonEmpty)
import GHC.HsToCore.Errors.Types


{- Note [TcRnDsMessage]
~~~~~~~~~~~~~~~~~~~~~

There is a mutual dependency between typecheck-rename and desugaring,
because we need to desugar expressions during TH splicing. This means that
while we are in the 'TcM' monad, we could get 'DsMessage'(s), and we also
need to run a desugar action inside the 'TcM' monad (see 'initDsTc'). However,
we still need to accumulate diagnostics from both phases, and therefore GHC
uses the same /shared/ IORef between these two monads. This is why we introduce
the 'TcRnDsMessage': we still want to grab onto the full accumulated diagnostics
(without leaving anything behind) but we also need a single, monomorphic type to
use in the 'IORef'.

-}


-- | Captures both desugar and typecheck errors.
-- See Note [TcRnDsMessage]
newtype TcRnDsMessage = TcRnDsMessage (Either DsMessage TcRnMessage)

mkTcRnDsMessage :: Either DsMessage TcRnMessage -> TcRnDsMessage
mkTcRnDsMessage = TcRnDsMessage

-- | An error which might arise during typechecking/renaming.
data TcRnMessage
  = TcRnUnknownMessage !DiagnosticMessage

  -- See 'mkDecoratedSDocAt' in 'GHC.Tc.Utils.Monad', where we need the 'UnitState'
  -- to render the 'Unit' properly. This is a type constructor to build an embellished
  -- 'DiagnosticMessage' which can be pretty-printed with the fully qualified 'UnitState'.
  | TcRnMessageWithUnitState !UnitState !DiagnosticMessage

  -- Errors thrown in GHC.Tc.Errors
  | TcRnBadTelescope
      [RenderableTyVarBndr] -- telescope
      [TyCoVar] -- sorted tyvars (in a correct order)
      !SDoc     -- context. TODO: Make it structured, eventually (#18516).
  | TcRnOutOfScope
      !RdrName -- name tried
      !OutOfScopeSuggestions -- similar name, import, etc suggestions
      !SDoc -- extra contents (see 'unboundNameX'). TODO: Make it structured, eventually (#18516).
      !SDoc -- context lines. TODO: Make it structured, eventually (#18516).
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
  | SuggestModulesDoNotExport (NonEmpty Module) !OccName
  | SuggestAddNameToImportLists !OccName (NonEmpty (Module, SrcSpan))
  | SuggestRemoveNameFromHidingLists !OccName (NonEmpty (Module, SrcSpan))

data ExtensionSuggestion = SuggestRecursiveDo
