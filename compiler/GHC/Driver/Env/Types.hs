{-# LANGUAGE DeriveFunctor #-}
module GHC.Driver.Env.Types
  ( Hsc(..)
  , HscEnv(..)
  ) where

import {-# SOURCE #-} GHC.Driver.Hooks
import GHC.Driver.Session ( DynFlags, HasDynFlags(..) )
import GHC.Prelude
import GHC.Runtime.Context
import GHC.Runtime.Interpreter.Types ( Interp )
import GHC.Types.Error ( WarningMessages )
import GHC.Types.Name.Cache
import GHC.Types.Target
import GHC.Types.TypeEnv
import GHC.Unit.External
import GHC.Unit.Finder.Types
import GHC.Unit.Home.ModInfo
import GHC.Unit.Module.Graph
import GHC.Unit.Env
import GHC.Unit.State
import GHC.Unit.Types
import GHC.Utils.Logger
import GHC.Utils.TmpFs
import {-# SOURCE #-} GHC.Driver.Plugins

import Control.Monad ( ap )
import Control.Monad.IO.Class
import Data.IORef

-- | The Hsc monad: Passing an environment and warning state
newtype Hsc a = Hsc (HscEnv -> WarningMessages -> IO (a, WarningMessages))
    deriving (Functor)

instance Applicative Hsc where
    pure a = Hsc $ \_ w -> return (a, w)
    (<*>) = ap

instance Monad Hsc where
    Hsc m >>= k = Hsc $ \e w -> do (a, w1) <- m e w
                                   case k a of
                                       Hsc k' -> k' e w1

instance MonadIO Hsc where
    liftIO io = Hsc $ \_ w -> do a <- io; return (a, w)

instance HasDynFlags Hsc where
    getDynFlags = Hsc $ \e w -> return (hsc_dflags e, w)

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

        hsc_HPT    :: HomePackageTable,
                -- ^ The home package table describes already-compiled
                -- home-package modules, /excluding/ the module we
                -- are compiling right now.
                -- (In one-shot mode the current module is the only
                -- home-package module, so hsc_HPT is empty.  All other
                -- modules count as \"external-package\" modules.
                -- However, even in GHCi mode, hi-boot interfaces are
                -- demand-loaded into the external-package table.)
                --
                -- 'hsc_HPT' is not mutable because we only demand-load
                -- external packages; the home package is eagerly
                -- loaded, module by module, by the compilation manager.
                --
                -- The HPT may contain modules compiled earlier by @--make@
                -- but not actually below the current module in the dependency
                -- graph.
                --
                -- (This changes a previous invariant: changed Jan 05.)

        hsc_EPS :: {-# UNPACK #-} !(IORef ExternalPackageState),
                -- ^ Information about the currently loaded external packages.
                -- This is mutable because packages will be demand-loaded during
                -- a compilation run as required.

        hsc_NC  :: {-# UNPACK #-} !(IORef NameCache),
                -- ^ As with 'hsc_EPS', this is side-effected by compiling to
                -- reflect sucking in interface files.  They cache the state of
                -- external interface files, in effect.

        hsc_FC   :: {-# UNPACK #-} !(IORef FinderCache),
                -- ^ The cached result of performing finding in the file system

        hsc_type_env_var :: Maybe (Module, IORef TypeEnv)
                -- ^ Used for one-shot compilation only, to initialise
                -- the 'IfGblEnv'. See 'GHC.Tc.Utils.tcg_type_env_var' for
                -- 'GHC.Tc.Utils.TcGblEnv'.  See also Note [hsc_type_env_var hack]

        , hsc_interp :: Maybe Interp
                -- ^ target code interpreter (if any) to use for TH and GHCi.
                -- See Note [Target code interpreter]

        , hsc_plugins :: ![LoadedPlugin]
                -- ^ plugins dynamically loaded after processing arguments. What
                -- will be loaded here is directed by DynFlags.pluginModNames.
                -- Arguments are loaded from DynFlags.pluginModNameOpts.
                --
                -- The purpose of this field is to cache the plugins so they
                -- don't have to be loaded each time they are needed.  See
                -- 'GHC.Runtime.Loader.initializePlugins'.

        , hsc_static_plugins :: ![StaticPlugin]
                -- ^ static plugins which do not need dynamic loading. These plugins are
                -- intended to be added by GHC API users directly to this list.
                --
                -- To add dynamically loaded plugins through the GHC API see
                -- 'addPluginModuleName' instead.

        , hsc_unit_dbs :: !(Maybe [UnitDatabase UnitId])
                -- ^ Stack of unit databases for the target platform.
                --
                -- This field is populated with the result of `initUnits`.
                --
                -- 'Nothing' means the databases have never been read from disk.
                --
                -- Usually we don't reload the databases from disk if they are
                -- cached, even if the database flags changed!

        , hsc_unit_env :: UnitEnv
                -- ^ Unit environment (unit state, home unit, etc.).
                --
                -- Initialized from the databases cached in 'hsc_unit_dbs' and
                -- from the DynFlags.

        , hsc_logger :: !Logger
                -- ^ Logger

        , hsc_hooks :: !Hooks
                -- ^ Hooks

        , hsc_tmpfs :: !TmpFs
                -- ^ Temporary files
 }

