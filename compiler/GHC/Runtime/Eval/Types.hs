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
import GHC.ByteCode.Types (InternalBreakpointId(..))
import GHC.Driver.Config (EvalStep(..))
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.TyThing
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
      { initiatedFrom :: Maybe SrcSpan
        -- ^ Step-out locations are filtered to make sure we don't stop at a
        -- continuation that is within the function from which step-out was
        -- initiated. See Note [Debugger: Step-out]
      }

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
enableGhcStepMode StepOut{}       = EvalStepOut
-- for the remaining step modes we need to stop at every single breakpoint.
enableGhcStepMode _               = EvalStepSingle

-- | Given a 'SingleStep' mode, whether the breakpoint was explicitly active,
-- and the SrcSpan of a breakpoint we hit, return @True@ if we should stop at
-- this breakpoint.
--
-- In particular, this will always be @False@ for @'RunToCompletion'@ and
-- @'RunAndLogSteps'@. We'd need further information e.g. about the user
-- breakpoints to determine whether to break in those modes.
breakHere :: Bool       -- ^ Was this breakpoint explicitly active (in the @BreakArray@s)?
          -> SingleStep -- ^ What kind of stepping were we doing
          -> SrcSpan    -- ^ The span of the breakpoint we hit
          -> Bool       -- ^ Should we stop here then?
breakHere b RunToCompletion _ = b
breakHere b RunAndLogSteps  _ = b
breakHere _ SingleStep      _ = True
breakHere b step break_span   = case step of
  LocalStep start_span  -> b || break_span `isSubspanOf` start_span
  ModuleStep start_span -> b || srcSpanFileName_maybe start_span == srcSpanFileName_maybe break_span
  StepOut Nothing       -> True
  StepOut (Just start)  ->
    -- See Note [Debugger: Filtering step-out stops]
    not (break_span `isSubspanOf` start)

{-
Note [Debugger: Filtering step-out stops]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Recall from Note [Debugger: Step-out] that the RTS explicitly enables the
breakpoint at the start of the first continuation frame on the stack, when
the step-out flag is set.

Often, the continuation on top of the stack will be part of the same function
from which step-out was initiated. A trivial example is a case expression:

  f x = case <brk>g x of ...

If we're stopped in <brk>, the continuation will be in the case alternatives rather
than in the function which called `f`. This is especially relevant for monadic
do-blocks which may end up being compiled to long chains of case expressions,
such as IO, and we don't want to stop at every line in the block while stepping out!

To make sure we only stop at a continuation outside of the current function, we
compare the continuation breakpoint `SrcSpan` against the current one. If the
continuation breakpoint is within the current function, instead of stopping, we
re-trigger step-out and return to the RTS interpreter right away.

This behaviour is very similar to `:steplocal`, which is implemented by
yielding from the RTS at every breakpoint (using `:step`) but only really
stopping when the breakpoint's `SrcSpan` is contained in the current function.

The function which determines if we should stop at the current breakpoint is
`breakHere`. For `StepOut`, `breakHere` will only return `True` if the
breakpoint is not contained in the function from which step-out was initiated.

Notably, this means we will ignore breakpoints enabled by the user if they are
contained in the function we are stepping out of.

If we had a way to distinguish whether a breakpoint was explicitly enabled (in
`BreakArrays`) by the user vs by step-out we could additionally break on
user-enabled breakpoints; however, it's not straightforward to determine this
and arguably it may be uncommon for a user to use step-out to run until the
next breakpoint in the same function. Of course, if a breakpoint in any other
function is hit before returning to the continuation, we will still stop there
(`breakHere` will be `True` because the break point is not within the initiator
function).
-}

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
                                        -- ^ the internal breakpoint we stopped at
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
