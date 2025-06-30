{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

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
   -- * InterpSymbolCache
   , InterpSymbolCache(..)
   , mkInterpSymbolCache
   , lookupInterpSymbolCache
   , updateInterpSymbolCache
   , purgeInterpSymbolCache
   , InterpSymbol(..)
   , SuffixOrInterpreted(..)
   , interpSymbolName
   , interpSymbolSuffix
   , eliminateInterpSymbol
   , interpretedInterpSymbol
   , interpreterProfiled
   , interpreterDynamic

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
   , WasmInterp
   , WasmInterpConfig (..)
   )
where

import GHC.Prelude
import GHC.Linker.Types

import GHCi.RemoteTypes
import GHCi.Message         ( Pipe )

import GHC.Platform
#if defined(HAVE_INTERNAL_INTERPRETER)
import GHC.Platform.Ways
#endif
import GHC.Utils.TmpFs
import GHC.Utils.Logger
import GHC.Unit.Env
import GHC.Unit.State
import GHC.Unit.Types
import GHC.StgToJS.Types
import GHC.StgToJS.Linker.Types
import GHC.Runtime.Interpreter.Types.SymbolCache

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

  , interpSymbolCache :: !InterpSymbolCache
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
  | ExtWasm !WasmInterp

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
type WasmInterp = ExtInterpState WasmInterpConfig ()

data InterpProcess = InterpProcess
  { interpPipe   :: !Pipe           -- ^ Pipe to communicate with the server
  , interpHandle :: !ProcessHandle  -- ^ Process handle of the server
  , interpLock   :: !(MVar ())      -- ^ Lock to prevent concurrent access to the stream
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

-- | Interpreter uses Profiling way
interpreterProfiled :: Interp -> Bool
interpreterProfiled interp = case interpInstance interp of
#if defined(HAVE_INTERNAL_INTERPRETER)
  InternalInterp     -> hostIsProfiled
#endif
  ExternalInterp ext -> case ext of
    ExtIServ i -> iservConfProfiled (interpConfig i)
    ExtJS {}   -> False -- we don't support profiling yet in the JS backend
    ExtWasm i -> wasmInterpProfiled $ interpConfig i

-- | Interpreter uses Dynamic way
interpreterDynamic :: Interp -> Bool
interpreterDynamic interp = case interpInstance interp of
#if defined(HAVE_INTERNAL_INTERPRETER)
  InternalInterp     -> hostIsDynamic
#endif
  ExternalInterp ext -> case ext of
    ExtIServ i -> iservConfDynamic (interpConfig i)
    ExtJS {}   -> False -- dynamic doesn't make sense for JS
    ExtWasm {} -> True  -- wasm dyld can only load dynamic code

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

------------------------
-- Wasm Stuff
------------------------

data WasmInterpConfig = WasmInterpConfig
  { wasmInterpDyLD           :: !FilePath  -- ^ Location of dyld.mjs script
  , wasmInterpLibDir         ::  FilePath  -- ^ wasi-sdk sysroot libdir containing libc.so, etc
  , wasmInterpOpts           :: ![String]  -- ^ Additional command line arguments for iserv

  -- wasm ghci browser mode
  , wasmInterpBrowser                      :: !Bool
  , wasmInterpBrowserHost                  :: !String
  , wasmInterpBrowserPort                  :: !Int
  , wasmInterpBrowserRedirectWasiConsole   :: !Bool
  , wasmInterpBrowserPuppeteerLaunchOpts   :: !(Maybe String)
  , wasmInterpBrowserPlaywrightBrowserType :: !(Maybe String)
  , wasmInterpBrowserPlaywrightLaunchOpts  :: !(Maybe String)

  , wasmInterpTargetPlatform :: !Platform
  , wasmInterpProfiled       :: !Bool      -- ^ Are we profiling yet?
  , wasmInterpHsSoSuffix     :: !String    -- ^ Shared lib filename common suffix sans .so, e.g. p-ghc9.13.20241001
  , wasmInterpUnitState      :: !UnitState
  }
