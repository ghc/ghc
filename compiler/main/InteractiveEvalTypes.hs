{-# LANGUAGE CPP #-}

-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2005-2007
--
-- Running statements interactively
--
-- -----------------------------------------------------------------------------

module InteractiveEvalTypes (
#ifdef GHCI
        Status(..), Resume(..), History(..), ExecResult(..),
        SingleStep(..), isStep, ExecOptions(..)
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

import Data.Word

data ExecOptions
 = ExecOptions
     { execSingleStep :: SingleStep         -- ^ stepping mode
     , execSourceFile :: String             -- ^ filename (for errors)
     , execLineNumber :: Int                -- ^ line number (for errors)
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
       { execResult :: Either SomeException [Name]
       , execAllocation :: Word64
       }
  | ExecBreak
       { breakThreadId :: ThreadId
       , breakNames :: [Name]
       , breakInfo :: Maybe BreakInfo
       }

data Status
   = Break Bool HValue BreakInfo ThreadId
          -- ^ the computation hit a breakpoint (Bool <=> was an exception)
   | Complete (Either SomeException [HValue]) Word64
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

