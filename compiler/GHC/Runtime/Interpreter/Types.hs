{-# LANGUAGE CPP #-}

-- | Types used by the runtime interpreter
module GHC.Runtime.Interpreter.Types
   ( Interp(..)
   , InterpInstance(..)
   , InterpProcess (..)
   , ExtInterp (..)
   , ExtInterpStatusVar
   , ExtInterpInstance (..)
   , ExtInterpState (..)
   , InterpStatus(..)
   -- * IServ
   , IServ
   , IServConfig(..)
   -- * JSInterp
   , JSInterp
   , JSInterpExtra (..)
   , JSInterpConfig (..)
   , JSState (..)
   , NodeJsSettings (..)
   , defaultNodeJsSettings
   )
where

import GHC.Prelude
import GHC.Linker.Types

import GHCi.RemoteTypes
import GHCi.Message         ( Pipe )
import GHC.Types.Unique.FM
import GHC.Data.FastString ( FastString )
import Foreign

import GHC.Utils.TmpFs
import GHC.Utils.Logger
import GHC.Unit.Env
import GHC.Unit.Types
import GHC.StgToJS.Types
import GHC.StgToJS.Linker.Types

import Control.Concurrent
import System.Process   ( ProcessHandle, CreateProcess )
import System.IO
import GHC.Unit.Finder.Types (FinderCache, FinderOpts)

-- | Interpreter
data Interp = Interp
  { interpInstance :: !InterpInstance
      -- ^ Interpreter instance (internal, external)

  , interpLoader   :: !Loader
      -- ^ Interpreter loader

  , interpLookupSymbolCache :: !(MVar (UniqFM FastString (Ptr ())))
      -- ^ LookupSymbol cache
  }

data InterpInstance
   = ExternalInterp !ExtInterp -- ^ External interpreter
#if defined(HAVE_INTERNAL_INTERPRETER)
   | InternalInterp            -- ^ Internal interpreter
#endif

data ExtInterp
  = ExtIServ !IServ
  | ExtJS !JSInterp

-- | External interpreter
--
-- The external interpreter is spawned lazily (on first use) to avoid slowing
-- down sessions that don't require it. The contents of the MVar reflects the
-- state of the interpreter (running or not).
data ExtInterpState cfg details = ExtInterpState
  { interpConfig :: !cfg
  , interpStatus :: !(ExtInterpStatusVar details)
  }

type ExtInterpStatusVar d = MVar (InterpStatus (ExtInterpInstance d))

type IServ    = ExtInterpState IServConfig    ()
type JSInterp = ExtInterpState JSInterpConfig JSInterpExtra

data InterpProcess = InterpProcess
  { interpPipe   :: !Pipe           -- ^ Pipe to communicate with the server
  , interpHandle :: !ProcessHandle  -- ^ Process handle of the server
  }

-- | Status of an external interpreter
data InterpStatus inst
   = InterpPending       -- ^ Not spawned yet
   | InterpRunning !inst -- ^ Running

-- | Configuration needed to spawn an external interpreter
data IServConfig = IServConfig
  { iservConfProgram  :: !String   -- ^ External program to run
  , iservConfOpts     :: ![String] -- ^ Command-line options
  , iservConfProfiled :: !Bool     -- ^ Use Profiling way
  , iservConfDynamic  :: !Bool     -- ^ Use Dynamic way
  , iservConfHook     :: !(Maybe (CreateProcess -> IO ProcessHandle)) -- ^ Hook
  , iservConfTrace    :: IO ()     -- ^ Trace action executed after spawn
  }

-- | Common field between native external interpreter and the JS one
data ExtInterpInstance c = ExtInterpInstance
  { instProcess       :: {-# UNPACK #-} !InterpProcess
      -- ^ External interpreter process and its pipe (communication channel)

  , instPendingFrees  :: !(MVar [HValueRef])
      -- ^ Values that need to be freed before the next command is sent.
      -- Finalizers for ForeignRefs can append values to this list
      -- asynchronously.

  , instExtra             :: !c
      -- ^ Instance specific extra fields
  }

------------------------
-- JS Stuff
------------------------

data JSInterpExtra = JSInterpExtra
  { instStdIn       :: !Handle         -- ^ Stdin for the process
  , instFinderCache :: !FinderCache
  , instFinderOpts  :: !FinderOpts
  , instJSState     :: !(MVar JSState) -- ^ Mutable state
  , instGhciUnitId  :: !UnitId         -- ^ GHCi unit-id
  }

data JSState = JSState
  { jsLinkState     :: !LinkPlan -- ^ Linker state of the interpreter
  , jsServerStarted :: !Bool     -- ^ Is the Haskell server started?
  }

-- | NodeJs configuration
data NodeJsSettings = NodeJsSettings
  { nodeProgram         :: FilePath        -- ^ location of node.js program
  , nodePath            :: Maybe FilePath  -- ^ value of NODE_PATH environment variable (search path for Node modules; GHCJS used to provide some)
  , nodeExtraArgs       :: [String]        -- ^ extra arguments to pass to node.js
  , nodeKeepAliveMaxMem :: Integer         -- ^ keep node.js (TH, GHCJSi) processes alive if they don't use more than this
  }

defaultNodeJsSettings :: NodeJsSettings
defaultNodeJsSettings = NodeJsSettings
  { nodeProgram         = "node"
  , nodePath            = Nothing
  , nodeExtraArgs       = []
  , nodeKeepAliveMaxMem = 536870912
  }


data JSInterpConfig = JSInterpConfig
  { jsInterpNodeConfig  :: !NodeJsSettings  -- ^ NodeJS settings
  , jsInterpScript      :: !FilePath        -- ^ Path to "ghc-interp.js" script
  , jsInterpTmpFs       :: !TmpFs
  , jsInterpTmpDir      :: !TempDir
  , jsInterpLogger      :: !Logger
  , jsInterpCodegenCfg  :: !StgToJSConfig
  , jsInterpUnitEnv     :: !UnitEnv
  , jsInterpFinderOpts  :: !FinderOpts
  , jsInterpFinderCache :: !FinderCache
  }

