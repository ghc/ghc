{-# LANGUAGE TypeFamilies #-}

-- | Definitions for writing /plugins/ for GHC. Plugins can hook into
-- several areas of the compiler. See the 'Plugin' type. These plugins
-- include type-checker plugins, source plugins, and core-to-core plugins.

module GHC.Plugins.Types (
      -- * Plugins
      Plugin(..)
    , FrontendPlugin(..)
    , StaticPlugin(..)
    , defaultPlugin
    , CommandLineOption

      -- ** Recompilation checking
    , purePlugin, impurePlugin, flagRecompile
    , PluginRecompile(..)

      -- * Plugin types
    , TFrontendPluginAction
    , TTypeCheckPlugin
    , TCorePlugin
    , THoleFitPlugin
    , TDynFlagsPlugin
    , TParseResultAction
    , TRenameResultAction
    , TTypeCheckResultAction
    , TSpliceRunAction
    , TInterfaceLoadAction

      -- * Internal
    , PluginWithArgs(..)
    , pluginRecompile'
    ) where

import GHC.Prelude

import GHC.Utils.Fingerprint
import Data.List (sort)
import GHC.Utils.Outputable (Outputable(..), text, (<+>))
import Data.Kind (Type)

--Qualified import so we can define a Semigroup instance
-- but it doesn't clash with Outputable.<>
import qualified Data.Semigroup

-- | Command line options gathered from the -PModule.Name:stuff syntax
-- are given to you as this type
type CommandLineOption = String

type family TTypeCheckPlugin       :: Type
type family TCorePlugin            :: Type
type family THoleFitPlugin         :: Type
type family TDynFlagsPlugin        :: Type
type family TParseResultAction     :: Type
type family TRenameResultAction    :: Type
type family TTypeCheckResultAction :: Type
type family TSpliceRunAction       :: Type
type family TInterfaceLoadAction   :: Type
type family TFrontendPluginAction  :: Type

-- | 'Plugin' is the compiler plugin data type. Try to avoid
-- constructing one of these directly, and just modify some fields of
-- 'defaultPlugin' instead: this is to try and preserve source-code
-- compatibility when we add fields to this.
--
-- Nonetheless, this API is preliminary and highly likely to change in
-- the future.
data Plugin = Plugin {
    installCoreToDos :: Maybe ([CommandLineOption] -> TCorePlugin)
    -- ^ Modify the Core pipeline that will be used for compilation.
    -- This is called as the Core pipeline is built for every module
    -- being compiled, and plugins get the opportunity to modify the
    -- pipeline in a nondeterministic order.

  , tcPlugin :: Maybe ([CommandLineOption] -> TTypeCheckPlugin)
    -- ^ An optional typechecker plugin, which may modify the
    -- behaviour of the constraint solver.

  , holeFitPlugin :: Maybe ([CommandLineOption] -> THoleFitPlugin)
    -- ^ An optional plugin to handle hole fits, which may re-order
    --   or change the list of valid hole fits and refinement hole fits.

  , dynflagsPlugin :: Maybe([CommandLineOption] -> TDynFlagsPlugin)
    -- ^ An optional plugin to update 'DynFlags', right after
    --   plugin loading. This can be used to register hooks
    --   or tweak any field of 'DynFlags' before doing
    --   actual work on a module.
    --
    --   @since 8.10.1

  , pluginRecompile :: [CommandLineOption] -> IO PluginRecompile
    -- ^ Specify how the plugin should affect recompilation.

  , parsedResultAction :: Maybe ([CommandLineOption] -> TParseResultAction)
    -- ^ Modify the module when it is parsed. This is called by
    -- "GHC.Driver.Main" when the parsing is successful.

  , renamedResultAction :: Maybe ([CommandLineOption] -> TRenameResultAction)
    -- ^ Modify each group after it is renamed. This is called after each
    -- `HsGroup` has been renamed.

  , typeCheckResultAction :: Maybe ([CommandLineOption] -> TTypeCheckResultAction)
    -- ^ Modify the module when it is type checked. This is called at the
    -- very end of typechecking.

  , spliceRunAction :: Maybe ([CommandLineOption] -> TSpliceRunAction)
    -- ^ Modify the TH splice or quasiqoute before it is run.

  , interfaceLoadAction :: Maybe ([CommandLineOption] -> TInterfaceLoadAction)
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

-- | A static plugin with its arguments. For registering compiled-in plugins
-- through the GHC API.
data StaticPlugin = StaticPlugin
  { spPlugin :: PluginWithArgs
  -- ^ the actual plugin together with its commandline arguments
  }

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
defaultPlugin = Plugin
   { installCoreToDos      = Nothing
   , tcPlugin              = Nothing
   , holeFitPlugin         = Nothing
   , dynflagsPlugin        = Nothing
   , renamedResultAction   = Nothing
   , parsedResultAction    = Nothing
   , typeCheckResultAction = Nothing
   , spliceRunAction       = Nothing
   , interfaceLoadAction   = Nothing
   , pluginRecompile       = impurePlugin
   }

data FrontendPlugin = FrontendPlugin
   { frontend :: TFrontendPluginAction
   }
