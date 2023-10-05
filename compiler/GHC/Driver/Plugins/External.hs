-- | External plugins
--
-- GHC supports two kinds of "static" plugins:
--  1. internal: setup with GHC-API
--  2. external: setup as explained below and loaded from shared libraries
--
-- The intended use case for external static plugins is with cross compilers: at
-- the time of writing, GHC is mono-target and a GHC cross-compiler (i.e. when
-- host /= target) can't build nor load plugins for the host using the
-- "non-static" plugin approach. Fixing this is tracked in #14335. If you're not
-- using a cross-compiler, you'd better use non-static plugins which are easier
-- to build and and safer to use (see below).
--
-- External static plugins can be configured via the command-line with
-- the -fplugin-library flag. Syntax is:
--
--   -fplugin-library=⟨file-path⟩;⟨unit-id⟩;⟨module⟩;⟨args⟩
--
-- Example:
--    -fplugin-library=path/to/plugin;package-123;Plugin.Module;["Argument","List"]
--
-- Building the plugin library:
--  1. link with the libraries used to build the compiler you target.  If you
--  target a cross-compiler (stage2), you can't directly use it to build the
--  plugin library. Use the stage1 compiler instead.
--
--  2. if you use cabal to build the library, its unit-id will be set by cabal
--  and will contain a hash (e.g. "my-plugin-unit-1345656546ABCDEF"). To force
--  the unit id, use GHC's `-this-unit-id` command line flag:
--    e.g. -this-unit-id my-plugin-unit
--  You can set this in the .cabal file of your library with the following
--  stanza: `ghc-options: -this-unit-id my-plugin-unit`
--
--  3. To make your plugin easier to distribute, you may want to link it
--  statically with all its dependencies. You would need to use `-shared`
--  without `-dynamic` when building your library.
--
--  However, all the static dependencies have to be built with `-fPIC` and it's
--  not done by default. See
--  https://www.hobson.space/posts/haskell-foreign-library/ for a way to modify
--  the compiler to do it.
--
--  In any case, don't link your plugin library statically with the RTS (e.g.
--  use `-fno-link-rts`) as there are some global variables in the RTS that must
--  be shared between the plugin and the compiler.
--
-- With external static plugins we don't check the type of the `plugin` closure
-- we look up. If it's not a valid `Plugin` value, it will probably crash badly.
--

module GHC.Driver.Plugins.External
  ( ExternalPluginSpec (..)
  , parseExternalPluginSpec
  )
where

import GHC.Prelude
import Text.Read

-- | External plugin spec
data ExternalPluginSpec = ExternalPluginSpec
  { esp_lib     :: !FilePath
  , esp_unit_id :: !String
  , esp_module  :: !String
  , esp_args    :: ![String]
  }

-- | Parser external static plugin specification from command-line flag
parseExternalPluginSpec :: String -> Maybe ExternalPluginSpec
parseExternalPluginSpec optflag =
  case break (== ';') optflag of
    (libPath, _:rest) -> case break (== ';') rest of
      (libName, _:pack) -> case break (== ';') pack of
        (modName, _:args) -> case readMaybe args of
          Just as -> Just (ExternalPluginSpec libPath libName modName as)
          Nothing -> Nothing
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing
