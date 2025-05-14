-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2005-2007
--
-- Running statements interactively
--
-- -----------------------------------------------------------------------------

module GHC.Runtime.Eval.Types (
        Resume(..), ResumeBindings, IcGlobalRdrEnv(..),
        History(..), ExecResult(..),
        SingleStep(..), enableGhcStepMode, breakHere,
        ExecOptions(..)
        ) where

import GHC.Prelude

import GHCi.RemoteTypes
import GHCi.Message (EvalExpr, ResumeContext)
import GHC.Driver.Config (EvalStep(..))
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.TyThing
import GHC.Types.Breakpoint
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Utils.Exception

import Data.Word
import GHC.Stack.CCS

data ExecOptions
 = ExecOptions
     { execSingleStep :: SingleStep         -- ^ stepping mode
     , execSourceFile :: String             -- ^ filename (for errors)
     , execLineNumber :: Int                -- ^ line number (for errors)
     , execWrap :: ForeignHValue -> EvalExpr ForeignHValue
     }

-- | What kind of stepping are we doing?
data SingleStep
   = RunToCompletion

   -- | :trace [expr]
   | RunAndLogSteps

   -- | :step [expr]
   | SingleStep

   -- | :stepout
   | StepOut

   -- | :steplocal [expr]
   | LocalStep
      { breakAt :: SrcSpan }

   -- | :stepmodule [expr]
   | ModuleStep
      { breakAt :: SrcSpan }

-- | Whether this 'SingleStep' mode requires instructing the interpreter to
-- step at every breakpoint or after every return (see @'EvalStep'@).
enableGhcStepMode :: SingleStep -> EvalStep
enableGhcStepMode RunToCompletion = EvalStepNone
enableGhcStepMode StepOut         = EvalStepOut
-- for the remaining step modes we need to stop at every single breakpoint.
enableGhcStepMode _               = EvalStepSingle

-- | Given a 'SingleStep' mode and the SrcSpan of a breakpoint we hit, return
-- @True@ if based on the step-mode alone we should stop at this breakpoint.
--
-- In particular, this will always be @False@ for @'RunToCompletion'@ and
-- @'RunAndLogSteps'@. We'd need further information e.g. about the user
-- breakpoints to determine whether to break in those modes.
breakHere :: SingleStep -> SrcSpan -> Bool
breakHere step break_span = case step of
  RunToCompletion -> False
  RunAndLogSteps  -> False
  StepOut         -> True
  SingleStep      -> True
  LocalStep  span -> break_span `isSubspanOf` span
  ModuleStep span -> srcSpanFileName_maybe span == srcSpanFileName_maybe break_span

data ExecResult

  -- | Execution is complete with either an exception or the list of
  -- user-visible names that were brought into scope.
  = ExecComplete
       { execResult :: Either SomeException [Name]
       , execAllocation :: Word64
       }

    -- | Execution stopped at a breakpoint.
    --
    -- Note: `ExecBreak` is only returned by `handleRunStatus` when GHCi should
    -- definitely stop at this breakpoint. GHCi is /not/ responsible for
    -- subsequently deciding whether to really stop here.
    -- `ExecBreak` always means GHCi breaks.
    | ExecBreak
       { breakNames   :: [Name]
       , breakPointId :: Maybe InternalBreakpointId
       }

-- | Essentially a GlobalRdrEnv, but with additional cached values to allow
-- efficient re-calculation when the imports change.
-- Fields are strict to avoid space leaks (see T4029)
-- All operations are in GHC.Runtime.Context.
-- See Note [icReaderEnv recalculation]
data IcGlobalRdrEnv = IcGlobalRdrEnv
  { igre_env :: !GlobalRdrEnv
    -- ^ The final environment
  , igre_prompt_env :: !GlobalRdrEnv
    -- ^ Just the things defined at the prompt (excluding imports!)
  }

data Resume = Resume
       { resumeStmt      :: String       -- the original statement
       , resumeContext   :: ForeignRef (ResumeContext [HValueRef])
       , resumeBindings  :: ResumeBindings
       , resumeFinalIds  :: [Id]         -- [Id] to bind on completion
       , resumeApStack   :: ForeignHValue -- The object from which we can get
                                        -- value of the free variables.
       , resumeBreakpointId :: Maybe InternalBreakpointId
                                        -- ^ the breakpoint we stopped at
                                        -- (Nothing <=> exception)
       , resumeSpan      :: SrcSpan     -- just a copy of the SrcSpan
                                        -- from the ModBreaks,
                                        -- otherwise it's a pain to
                                        -- fetch the ModDetails &
                                        -- ModBreaks to get this.
       , resumeDecl      :: String       -- ditto
       , resumeCCS       :: RemotePtr CostCentreStack
       , resumeHistory   :: [History]
       , resumeHistoryIx :: Int           -- 0 <==> at the top of the history
       }

type ResumeBindings = ([TyThing], IcGlobalRdrEnv)

data History = History
  { historyApStack        :: ForeignHValue
  , historyBreakpointId   :: InternalBreakpointId -- ^ breakpoint identifier
  , historyEnclosingDecls :: [String]             -- ^ declarations enclosing the breakpoint
  }
