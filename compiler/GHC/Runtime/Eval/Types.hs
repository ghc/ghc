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
        SingleStep(..), isStep, ExecOptions(..)
        ) where

import GHC.Prelude

import GHCi.RemoteTypes
import GHCi.Message (EvalExpr, ResumeContext)
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.TyThing
import GHC.Types.BreakInfo
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc

import Data.Word
import GHC.Stack.CCS

data ExecOptions
 = ExecOptions
     { execSingleStep :: SingleStep         -- ^ stepping mode
     , execSourceFile :: String             -- ^ filename (for errors)
     , execLineNumber :: Int                -- ^ line number (for errors)
     , execWrap :: ForeignHValue -> EvalExpr ForeignHValue
     }

data SingleStep
   = RunToCompletion
   | SingleStep
   | RunAndLogSteps

isStep :: SingleStep -> Bool
isStep RunToCompletion = False
isStep _ = True

data ExecResult
  = ExecComplete
       { execResult :: Either SomeExceptionWithLocation [Name]
       , execAllocation :: Word64
       }
  | ExecBreak
       { breakNames :: [Name]
       , breakInfo :: Maybe BreakInfo
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
       , resumeBreakInfo :: Maybe BreakInfo
                                        -- the breakpoint we stopped at
                                        -- (module, index)
                                        -- (Nothing <=> exception)
       , resumeSpan      :: SrcSpan      -- just a copy of the SrcSpan
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

data History
   = History {
        historyApStack   :: ForeignHValue,
        historyBreakInfo :: BreakInfo,
        historyEnclosingDecls :: [String]  -- declarations enclosing the breakpoint
   }
