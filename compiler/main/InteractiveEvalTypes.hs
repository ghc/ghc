-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2005-2007
--
-- Running statements interactively
--
-- -----------------------------------------------------------------------------

module InteractiveEvalTypes (
#ifdef GHCI
        RunResult(..), Status(..), Resume(..), History(..),
#endif
        ) where

#ifdef GHCI

import Id
import BasicTypes
import Name
import RdrName
import TypeRep
import ByteCodeInstr
import SrcLoc
import Exception
import Control.Concurrent

data RunResult
  = RunOk [Name]                -- ^ names bound by this evaluation
  | RunException SomeException  -- ^ statement raised an exception
  | RunBreak ThreadId [Name] (Maybe BreakInfo)

data Status
   = Break Bool HValue BreakInfo ThreadId
          -- ^ the computation hit a breakpoint (Bool <=> was an exception)
   | Complete (Either SomeException [HValue])
          -- ^ the computation completed with either an exception or a value

data Resume
   = Resume {
       resumeStmt      :: String,       -- the original statement
       resumeThreadId  :: ThreadId,     -- thread running the computation
       resumeBreakMVar :: MVar (),
       resumeStatMVar  :: MVar Status,
       resumeBindings  :: ([TyThing], GlobalRdrEnv),
       resumeFinalIds  :: [Id],         -- [Id] to bind on completion
       resumeApStack   :: HValue,       -- The object from which we can get
                                        -- value of the free variables.
       resumeBreakInfo :: Maybe BreakInfo,
                                        -- the breakpoint we stopped at
                                        -- (Nothing <=> exception)
       resumeSpan      :: SrcSpan,      -- just a cache, otherwise it's a pain
                                        -- to fetch the ModDetails & ModBreaks
                                        -- to get this.
       resumeHistory   :: [History],
       resumeHistoryIx :: Int           -- 0 <==> at the top of the history
   }

data History
   = History {
        historyApStack   :: HValue,
        historyBreakInfo :: BreakInfo,
        historyEnclosingDecls :: [String]  -- declarations enclosing the breakpoint
   }
#endif

