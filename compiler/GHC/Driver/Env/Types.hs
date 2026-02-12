{-# LANGUAGE DerivingVia #-}

module GHC.Driver.Env.Types
  ( Hsc(..)
  , HscEnv(..)
  , HasHscEnv(..)
  , FinderEnv(..)
  , FinderScope(..)
  , HomeOrExternal(..)
  , IfaceLoadEnv(..)
  , IfaceLoadScope(..)
  ) where

import GHC.Driver.Errors.Types ( GhcMessage )
import {-# SOURCE #-} GHC.Driver.Hooks
import GHC.Driver.DynFlags ( ContainsDynFlags(..), HasDynFlags(..), DynFlags )
import GHC.Driver.LlvmConfigCache (LlvmConfigCache)

import GHC.Prelude
import GHC.Runtime.Context
import GHC.Runtime.Interpreter.Types ( Interp )
import GHC.Types.Error ( Messages )
import GHC.Types.Name.Cache
import GHC.Types.Target
import GHC.Types.TypeEnv
import GHC.Unit.Finder.Types (FinderCache, FinderOpts)
import GHC.Unit.Env
import GHC.Unit.External (ExternalUnitCache)
import GHC.Unit.State (UnitState)
import GHC.Unit.Home
import GHC.Unit.Home.Graph (UnitEnvGraph)
import GHC.Unit.Types (UnitId)
import GHC.Utils.Logger
import GHC.Utils.TmpFs
import {-# SOURCE #-} GHC.Driver.Plugins

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.IORef
import qualified Data.Set as Set
import GHC.Driver.Env.KnotVars

-- | The Hsc monad: Passing an environment and diagnostic state
newtype Hsc a = Hsc (HscEnv -> Messages GhcMessage -> IO (a, Messages GhcMessage))
    deriving (Functor, Applicative, Monad, MonadIO)
      via ReaderT HscEnv (StateT (Messages GhcMessage) IO)

instance HasHscEnv ((->) HscEnv) where
    getHscEnv = id

instance HasHscEnv Hsc where
    getHscEnv = Hsc $ \e w -> return (e, w)

instance HasDynFlags Hsc where
    getDynFlags = Hsc $ \e w -> return (hsc_dflags e, w)

instance ContainsDynFlags HscEnv where
    extractDynFlags h = hsc_dflags h

instance ContainsHooks HscEnv where
    extractHooks = hsc_hooks

instance ContainsLogger HscEnv where
    extractLogger = hsc_logger

instance ContainsNameCache HscEnv where
    extractNameCache = hsc_NC

instance HasLogger Hsc where
    getLogger = Hsc $ \e w -> return (hsc_logger e, w)


-- | HscEnv is like 'GHC.Driver.Monad.Session', except that some of the fields are immutable.
-- An HscEnv is used to compile a single module from plain Haskell source
-- code (after preprocessing) to either C, assembly or C--. It's also used
-- to store the dynamic linker state to allow for multiple linkers in the
-- same address space.
-- Things like the module graph don't change during a single compilation.
--
-- Historical note: \"hsc\" used to be the name of the compiler binary,
-- when there was a separate driver and compiler.  To compile a single
-- module, the driver would invoke hsc on the source code... so nowadays
-- we think of hsc as the layer of the compiler that deals with compiling
-- a single module.
data HscEnv
  = HscEnv {
        hsc_dflags :: DynFlags,
                -- ^ The dynamic flag settings

        hsc_targets :: [Target],
                -- ^ The targets (or roots) of the current session

        hsc_IC :: InteractiveContext,
                -- ^ The context for evaluating interactive statements

        hsc_NC  :: {-# UNPACK #-} !NameCache,
                -- ^ Global Name cache so that each Name gets a single Unique.
                -- Also track the origin of the Names.

        hsc_FC   :: {-# UNPACK #-} !FinderCache,
                -- ^ The cached result of performing finding in the file system

        hsc_type_env_vars :: KnotVars (IORef TypeEnv)
                -- ^ Used for one-shot compilation only, to initialise
                -- the 'IfGblEnv'. See 'GHC.Tc.Utils.tcg_type_env_var' for
                -- 'GHC.Tc.Utils.TcGblEnv'.  See also Note [hsc_type_env_var hack]

        , hsc_interp :: Maybe Interp
                -- ^ target code interpreter (if any) to use for TH and GHCi.
                -- See Note [Target code interpreter]

        , hsc_plugins :: !Plugins
                -- ^ Plugins

        , hsc_unit_env :: UnitEnv
                -- ^ Unit environment (unit state, home unit, etc.).
                --
                -- Initialized from the databases cached in 'hsc_unit_dbs' and
                -- from the DynFlags.

        , hsc_logger :: !Logger
                -- ^ Logger with its flags.
                --
                -- Don't forget to update the logger flags if the logging
                -- related DynFlags change. Or better, use hscSetFlags setter
                -- which does it.

        , hsc_hooks :: !Hooks
                -- ^ Hooks

        , hsc_tmpfs :: !TmpFs
                -- ^ Temporary files

        , hsc_llvm_config :: !LlvmConfigCache
                -- ^ LLVM configuration cache.
 }

data HomeOrExternal = Home | External

-- TODO: Not sure that this type index is worthwhile.
-- This is just so that `findImportedModule` doesn't have to deal with the external case.
data FinderScope (a :: HomeOrExternal) where
  FinderScopeHome ::
      { finder_scope_home_unit  :: !(HomeUnit)
      , finder_scope_other_opts :: !(UnitEnvGraph (UnitState, FinderOpts))
      } -> FinderScope Home
  FinderScopeExternalOnly ::
       { finder_scope_external_home_unit :: !(Maybe HomeUnit)
       -- ^ This is Just when we are in oneshot mode and try to find home package things in the EPS
       } -> FinderScope External

data FinderEnv a = FinderEnv
  { finder_cache      :: !FinderCache
  , finder_opts       :: !FinderOpts
  , finder_unit_state :: !UnitState
  , finder_scope      :: !(FinderScope a)
  }

-- | Describes whether interface loading is allowed to consult the
-- home-package table (HPT) or if we're in an external-only mode (see
-- 'dontLeakTheHUG').
data IfaceLoadScope
  = IfaceLoadScopeHome !HomeUnitGraph
  | IfaceLoadScopeExternalOnly

data IfaceLoadEnv = forall a . IfaceLoadEnv
  { ifle_home_unit       :: !HomeUnit
  , ifle_home_unit_maybe :: !(Maybe HomeUnit)
  , ifle_dflags          :: !DynFlags
  , ifle_all_home_unit_ids :: !(Set.Set UnitId)
  , ifle_plugins         :: !Plugins -- This is just for iface load action
  , ifle_hooks           :: !Hooks   -- Not sure
  , ifle_logger          :: !Logger  -- It's the logger
  , ifle_name_cache      :: !NameCache -- It's the namecache
  , ifle_unit_state      :: !UnitState -- UnitState of home unit which started the load of external package
  , ifle_eps_cache       :: !ExternalUnitCache -- Where external interfaces live
  , ifle_type_env_vars   :: !(KnotVars (IORef TypeEnv)) -- This is needed when calling tcIface in oneshot mode
  , ifle_finder_env      :: !(FinderEnv a) -- Environment needed by the finder component
  , ifle_load_scope      :: !IfaceLoadScope -- Current loading scope (HPT vs External)
  , ifle_module_graph_has_holes :: !Bool    -- Whether we are in some backpack session and need to leak HPTs everywhere.
  }

instance ContainsDynFlags IfaceLoadEnv where
  extractDynFlags = ifle_dflags

instance ContainsHooks IfaceLoadEnv where
  extractHooks = ifle_hooks

instance ContainsLogger IfaceLoadEnv where
  extractLogger = ifle_logger

instance ContainsNameCache IfaceLoadEnv where
  extractNameCache = ifle_name_cache

class HasHscEnv m where
    getHscEnv :: m HscEnv
