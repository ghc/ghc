{-# LANGUAGE CPP #-}

-- | Types used by the runtime interpreter
module GHC.Runtime.Interpreter.Types
   ( Interp(..)
   , IServ(..)
   , IServInstance(..)
   , IServConfig(..)
   , IServState(..)
   )
where

import GHC.Prelude

import GHCi.RemoteTypes
import GHCi.Message         ( Pipe )
import GHC.Types.Unique.FM
import GHC.Data.FastString ( FastString )
import Foreign

import Control.Concurrent
import System.Process   ( ProcessHandle, CreateProcess )

-- | Runtime interpreter
data Interp
   = ExternalInterp !IServConfig !IServ -- ^ External interpreter
#if defined(HAVE_INTERNAL_INTERPRETER)
   | InternalInterp                     -- ^ Internal interpreter
#endif

-- | External interpreter
--
-- The external interpreter is spawned lazily (on first use) to avoid slowing
-- down sessions that don't require it. The contents of the MVar reflects the
-- state of the interpreter (running or not).
newtype IServ = IServ (MVar IServState)

-- | State of an external interpreter
data IServState
   = IServPending                 -- ^ Not spawned yet
   | IServRunning !IServInstance  -- ^ Running

-- | Configuration needed to spawn an external interpreter
data IServConfig = IServConfig
  { iservConfProgram  :: !String   -- ^ External program to run
  , iservConfOpts     :: ![String] -- ^ Command-line options
  , iservConfProfiled :: !Bool     -- ^ Use Profiling way
  , iservConfDynamic  :: !Bool     -- ^ Use Dynamic way
  , iservConfHook     :: !(Maybe (CreateProcess -> IO ProcessHandle)) -- ^ Hook
  , iservConfTrace    :: IO ()     -- ^ Trace action executed after spawn
  }

-- | External interpreter instance
data IServInstance = IServInstance
  { iservPipe              :: !Pipe
  , iservProcess           :: !ProcessHandle
  , iservLookupSymbolCache :: !(UniqFM FastString (Ptr ()))
  , iservPendingFrees      :: ![HValueRef]
      -- ^ Values that need to be freed before the next command is sent.
      -- Threads can append values to this list asynchronously (by modifying the
      -- IServ state MVar).
  }

