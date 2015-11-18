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
        Resume(..), History(..), ExecResult(..),
        SingleStep(..), isStep, ExecOptions(..)
#endif
        ) where

#ifdef GHCI

import GHCi.RemoteTypes (ForeignHValue)
import GHCi.Message (EvalExpr)
import Id
import Name
import RdrName
import Type
import ByteCodeTypes
import SrcLoc
import Exception

import Data.Word

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
       { execResult :: Either SomeException [Name]
       , execAllocation :: Word64
       }
  | ExecBreak
       { breakNames :: [Name]
       , breakInfo :: Maybe BreakInfo
       }

data Resume
   = Resume {
       resumeStmt      :: String,       -- the original statement
       resumeContext   :: ForeignHValue, -- thread running the computation
       resumeBindings  :: ([TyThing], GlobalRdrEnv),
       resumeFinalIds  :: [Id],         -- [Id] to bind on completion
       resumeApStack   :: ForeignHValue, -- The object from which we can get
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
        historyApStack   :: ForeignHValue,
        historyBreakInfo :: BreakInfo,
        historyEnclosingDecls :: [String]  -- declarations enclosing the breakpoint
   }
#endif

