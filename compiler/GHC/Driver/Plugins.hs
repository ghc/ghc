{-# LANGUAGE RankNTypes #-}


-- | Definitions for writing /plugins/ for GHC. Plugins can hook into
-- several areas of the compiler. See the 'Plugin' type. These plugins
-- include type-checker plugins, source plugins, and core-to-core plugins.

module GHC.Driver.Plugins (
      -- * Plugins
      Plugins (..)
    , emptyPlugins
    , Plugin(..)
    , defaultPlugin
    , CommandLineOption
    , PsMessages(..)
    , ParsedResult(..)
      -- ** Recompilation checking
    , purePlugin, impurePlugin, flagRecompile
    , PluginRecompile(..)

      -- * Plugin types
      -- ** Frontend plugins
    , FrontendPlugin(..), defaultFrontendPlugin, FrontendPluginAction
      -- ** Core plugins
      -- | Core plugins allow plugins to register as a Core-to-Core pass.
    , CorePlugin
      -- ** Typechecker plugins
      -- | Typechecker plugins allow plugins to provide evidence to the
      -- typechecker.
    , TcPlugin
      -- ** Source plugins
      -- | GHC offers a number of points where plugins can access and modify its
      -- front-end (\"source\") representation. These include:
      --
      -- - access to the parser result with 'parsedResultAction'
      -- - access to the renamed AST with 'renamedResultAction'
      -- - access to the typechecked AST with 'typeCheckResultAction'
      -- - access to the Template Haskell splices with 'spliceRunAction'
      -- - access to loaded interface files with 'interfaceLoadAction'
      --
    , keepRenamedSource
      -- ** Defaulting plugins
      -- | Defaulting plugins can add candidate types to the defaulting
      -- mechanism.
    , DefaultingPlugin
      -- ** Hole fit plugins
      -- | hole fit plugins allow plugins to change the behavior of valid hole
      -- fit suggestions
    , HoleFitPluginR

      -- * Internal
    , PluginWithArgs(..), pluginsWithArgs, pluginRecompile'
    , LoadedPlugin(..), lpModuleName
    , StaticPlugin(..)
    , mapPlugins, withPlugins, withPlugins_
    ) where

import GHC.Prelude

import GHC.Driver.Env
import GHC.Driver.Monad
import GHC.Driver.Phases

import GHC.Unit.Module
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModSummary

import GHC.Parser.Errors.Types (PsWarning, PsError)

import qualified GHC.Tc.Types
import GHC.Tc.Types ( TcGblEnv, IfM, TcM, tcg_rn_decls, tcg_rn_exports  )
import GHC.Tc.Errors.Hole.FitTypes ( HoleFitPluginR )

import GHC.Core.Opt.Monad ( CoreToDo, CoreM )
import GHC.Hs
import GHC.Types.Error (Messages)
import GHC.Utils.Fingerprint
import GHC.Utils.Outputable (Outputable(..), text, (<+>))

import Data.List (sort)

--Qualified import so we can define a Semigroup instance
-- but it doesn't clash with Outputable.<>
import qualified Data.Semigroup

import Control.Monad
import GHC.Linker.Types
import GHC.Types.Unique.DFM

-- | Command line options gathered from the -PModule.Name:stuff syntax
-- are given to you as this type
type CommandLineOption = String

-- | Errors and warnings produced by the parser
data PsMessages = PsMessages { psWarnings :: Messages PsWarning
                             , psErrors   :: Messages PsError
                             }

-- | Result of running the parser and the parser plugin
data ParsedResult = ParsedResult
  { -- | Parsed module, potentially modified by a plugin
    parsedResultModule :: HsParsedModule
  , -- | Warnings and errors from parser, potentially modified by a plugin
    parsedResultMessages :: PsMessages
  }

-- | 'Plugin' is the compiler plugin data type. Try to avoid
-- constructing one of these directly, and just modify some fields of
-- 'defaultPlugin' instead: this is to try and preserve source-code
-- compatibility when we add fields to this.
--
-- Nonetheless, this API is preliminary and highly likely to change in
-- the future.
data Plugin = Plugin {
    installCoreToDos :: CorePlugin
    -- ^ Modify the Core pipeline that will be used for compilation.
    -- This is called as the Core pipeline is built for every module
    -- being compiled, and plugins get the opportunity to modify the
    -- pipeline in a nondeterministic order.
  , tcPlugin :: TcPlugin
    -- ^ An optional typechecker plugin, which may modify the
    -- behaviour of the constraint solver.
  , defaultingPlugin :: DefaultingPlugin
    -- ^ An optional defaulting plugin, which may specify the
    -- additional type-defaulting rules.
  , holeFitPlugin :: HoleFitPlugin
    -- ^ An optional plugin to handle hole fits, which may re-order
    --   or change the list of valid hole fits and refinement hole fits.

  , driverPlugin :: [CommandLineOption] -> HscEnv -> IO HscEnv
    -- ^ An optional plugin to update 'HscEnv', right after plugin loading. This
    -- can be used to register hooks or tweak any field of 'DynFlags' before
    -- doing actual work on a module.
    --
    --   @since 8.10.1

  , pluginRecompile :: [CommandLineOption] -> IO PluginRecompile
    -- ^ Specify how the plugin should affect recompilation.
  , parsedResultAction :: [CommandLineOption] -> ModSummary
                       -> ParsedResult -> Hsc ParsedResult
    -- ^ Modify the module when it is parsed. This is called by
    -- "GHC.Driver.Main" when the parser has produced no or only non-fatal
    -- errors.
    -- Compilation will fail if the messages produced by this function contain
    -- any errors.
  , renamedResultAction :: [CommandLineOption] -> TcGblEnv
                                -> HsGroup GhcRn -> TcM (TcGblEnv, HsGroup GhcRn)
    -- ^ Modify each group after it is renamed. This is called after each
    -- `HsGroup` has been renamed.
  , typeCheckResultAction :: [CommandLineOption] -> ModSummary -> TcGblEnv
                               -> TcM TcGblEnv
    -- ^ Modify the module when it is type checked. This is called at the
    -- very end of typechecking.
  , spliceRunAction :: [CommandLineOption] -> LHsExpr GhcTc
                         -> TcM (LHsExpr GhcTc)
    -- ^ Modify the TH splice or quasiqoute before it is run.
  , interfaceLoadAction :: forall lcl . [CommandLineOption] -> ModIface
                                          -> IfM lcl ModIface
    -- ^ Modify an interface that have been loaded. This is called by
    -- "GHC.Iface.Load" when an interface is successfully loaded. Not applied to
    -- the loading of the plugin interface. Tools that rely on information from
    -- modules other than the currently compiled one should implement this
    -- function.
  }

-- Note [Source plugins]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The `Plugin` datatype have been extended by fields that allow access to the
-- different inner representations that are generated during the compilation
-- process. These fields are `parsedResultAction`, `renamedResultAction`,
-- `typeCheckResultAction`, `spliceRunAction` and `interfaceLoadAction`.
--
-- The main purpose of these plugins is to help tool developers. They allow
-- development tools to extract the information about the source code of a big
-- Haskell project during the normal build procedure. In this case the plugin
-- acts as the tools access point to the compiler that can be controlled by
-- compiler flags. This is important because the manipulation of compiler flags
-- is supported by most build environment.
--
-- For the full discussion, check the full proposal at:
-- https://gitlab.haskell.org/ghc/ghc/wikis/extended-plugins-proposal

data PluginWithArgs = PluginWithArgs
  { paPlugin :: Plugin
    -- ^ the actual callable plugin
  , paArguments :: [CommandLineOption]
    -- ^ command line arguments for the plugin
  }

-- | A plugin with its arguments. The result of loading the plugin.
data LoadedPlugin = LoadedPlugin
  { lpPlugin :: PluginWithArgs
  -- ^ the actual plugin together with its commandline arguments
  , lpModule :: ModIface
  -- ^ the module containing the plugin
  }

-- | A static plugin with its arguments. For registering compiled-in plugins
-- through the GHC API.
data StaticPlugin = StaticPlugin
  { spPlugin :: PluginWithArgs
  -- ^ the actual plugin together with its commandline arguments
  }

lpModuleName :: LoadedPlugin -> ModuleName
lpModuleName = moduleName . mi_module . lpModule

pluginRecompile' :: PluginWithArgs -> IO PluginRecompile
pluginRecompile' (PluginWithArgs plugin args) = pluginRecompile plugin args

data PluginRecompile = ForceRecompile | NoForceRecompile | MaybeRecompile Fingerprint

instance Outputable PluginRecompile where
  ppr ForceRecompile = text "ForceRecompile"
  ppr NoForceRecompile = text "NoForceRecompile"
  ppr (MaybeRecompile fp) = text "MaybeRecompile" <+> ppr fp

instance Semigroup PluginRecompile where
  ForceRecompile <> _ = ForceRecompile
  NoForceRecompile <> r = r
  MaybeRecompile fp <> NoForceRecompile   = MaybeRecompile fp
  MaybeRecompile fp <> MaybeRecompile fp' = MaybeRecompile (fingerprintFingerprints [fp, fp'])
  MaybeRecompile _fp <> ForceRecompile     = ForceRecompile

instance Monoid PluginRecompile where
  mempty = NoForceRecompile

type CorePlugin = [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
type TcPlugin = [CommandLineOption] -> Maybe GHC.Tc.Types.TcPlugin
type DefaultingPlugin = [CommandLineOption] -> Maybe GHC.Tc.Types.DefaultingPlugin
type HoleFitPlugin = [CommandLineOption] -> Maybe HoleFitPluginR

purePlugin, impurePlugin, flagRecompile :: [CommandLineOption] -> IO PluginRecompile
purePlugin _args = return NoForceRecompile

impurePlugin _args = return ForceRecompile

flagRecompile =
  return . MaybeRecompile . fingerprintFingerprints . map fingerprintString . sort

-- | Default plugin: does nothing at all, except for marking that safe
-- inference has failed unless @-fplugin-trustworthy@ is passed. For
-- compatibility reason you should base all your plugin definitions on this
-- default value.
defaultPlugin :: Plugin
defaultPlugin = Plugin {
        installCoreToDos      = const return
      , tcPlugin              = const Nothing
      , defaultingPlugin      = const Nothing
      , holeFitPlugin         = const Nothing
      , driverPlugin          = const return
      , pluginRecompile       = impurePlugin
      , renamedResultAction   = \_ env grp -> return (env, grp)
      , parsedResultAction    = \_ _ -> return
      , typeCheckResultAction = \_ _ -> return
      , spliceRunAction       = \_ -> return
      , interfaceLoadAction   = \_ -> return
    }


-- | A renamer plugin which mades the renamed source available in
-- a typechecker plugin.
keepRenamedSource :: [CommandLineOption] -> TcGblEnv
                  -> HsGroup GhcRn -> TcM (TcGblEnv, HsGroup GhcRn)
keepRenamedSource _ gbl_env group =
  return (gbl_env { tcg_rn_decls = update (tcg_rn_decls gbl_env)
                  , tcg_rn_exports = update_exports (tcg_rn_exports gbl_env) }, group)
  where
    update_exports Nothing = Just []
    update_exports m = m

    update Nothing = Just emptyRnGroup
    update m       = m


type PluginOperation m a = Plugin -> [CommandLineOption] -> a -> m a
type ConstPluginOperation m a = Plugin -> [CommandLineOption] -> a -> m ()

data Plugins = Plugins
  { staticPlugins :: ![StaticPlugin]
      -- ^ Static plugins which do not need dynamic loading. These plugins are
      -- intended to be added by GHC API users directly to this list.
      --
      -- To add dynamically loaded plugins through the GHC API see
      -- 'addPluginModuleName' instead.

  , loadedPlugins :: ![LoadedPlugin]
      -- ^ Plugins dynamically loaded after processing arguments. What
      -- will be loaded here is directed by DynFlags.pluginModNames.
      -- Arguments are loaded from DynFlags.pluginModNameOpts.
      --
      -- The purpose of this field is to cache the plugins so they
      -- don't have to be loaded each time they are needed.  See
      -- 'GHC.Runtime.Loader.initializePlugins'.
  , loadedPluginDeps :: !([Linkable], PkgsLoaded)
  -- ^ The object files required by the loaded plugins
  -- See Note [Plugin dependencies]
  }

emptyPlugins :: Plugins
emptyPlugins = Plugins [] [] ([], emptyUDFM)


pluginsWithArgs :: Plugins -> [PluginWithArgs]
pluginsWithArgs plugins =
  map lpPlugin (loadedPlugins plugins) ++
  map spPlugin (staticPlugins plugins)

-- | Perform an operation by using all of the plugins in turn.
withPlugins :: Monad m => Plugins -> PluginOperation m a -> a -> m a
withPlugins plugins transformation input = foldM go input (pluginsWithArgs plugins)
  where
    go arg (PluginWithArgs p opts) = transformation p opts arg

mapPlugins :: Plugins -> (Plugin -> [CommandLineOption] -> a) -> [a]
mapPlugins plugins f = map (\(PluginWithArgs p opts) -> f p opts) (pluginsWithArgs plugins)

-- | Perform a constant operation by using all of the plugins in turn.
withPlugins_ :: Monad m => Plugins -> ConstPluginOperation m a -> a -> m ()
withPlugins_ plugins transformation input
  = mapM_ (\(PluginWithArgs p opts) -> transformation p opts input)
          (pluginsWithArgs plugins)

type FrontendPluginAction = [String] -> [(String, Maybe Phase)] -> Ghc ()
data FrontendPlugin = FrontendPlugin {
      frontend :: FrontendPluginAction
    }
defaultFrontendPlugin :: FrontendPlugin
defaultFrontendPlugin = FrontendPlugin { frontend = \_ _ -> return () }
