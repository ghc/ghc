{-# LANGUAGE DerivingVia #-}

module GHC.Driver.Env.Types
  ( Hsc(..)
  , HscEnv(..)
  ) where

import GHC.Driver.Errors.Types ( GhcMessage )
import {-# SOURCE #-} GHC.Driver.Hooks
import GHC.Driver.Session ( ContainsDynFlags(..), HasDynFlags(..), DynFlags )
import GHC.Prelude
import GHC.Runtime.Context
import GHC.Runtime.Interpreter.Types ( Interp )
import GHC.Types.Error ( Messages )
import GHC.Types.Name.Cache
import GHC.Types.Target
import GHC.Types.TypeEnv
import GHC.Unit.Finder.Types
import GHC.Unit.Module.Graph
import GHC.Unit.Env
import GHC.Utils.Logger
import GHC.Utils.TmpFs
import {-# SOURCE #-} GHC.Driver.Plugins

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.IORef
import GHC.Driver.Env.KnotVars

-- | The Hsc monad: Passing an environment and diagnostic state
newtype Hsc a = Hsc (HscEnv -> Messages GhcMessage -> IO (a, Messages GhcMessage))
    deriving (Functor, Applicative, Monad, MonadIO)
      via ReaderT HscEnv (StateT (Messages GhcMessage) IO)

instance HasDynFlags Hsc where
    getDynFlags = Hsc $ \e w -> return (hsc_dflags e, w)

instance ContainsDynFlags HscEnv where
    extractDynFlags h = hsc_dflags h

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

        hsc_mod_graph :: ModuleGraph,
                -- ^ The module graph of the current session

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
 }
