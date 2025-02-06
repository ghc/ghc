module GHC.Driver.Config.Parser
  ( initParserOpts
  , supportedLanguagePragmas

  , predefinedMacros
  )
where

import GHC.Prelude
import GHC.Platform

import qualified Data.Map as Map
import Data.Maybe

import GHC.Driver.Session
import GHC.Driver.Config.Diagnostic

import GHC.Parser.Lexer
import GHC.Parser.PreProcess.State (MacroName (..), MacroDef)

-- | Extracts the flags needed for parsing
initParserOpts :: DynFlags -> ParserOpts
initParserOpts =
  mkParserOpts
    <$> extensionFlags
    <*> initDiagOpts
    <*> safeImportsOn
    <*> gopt Opt_Haddock
    <*> gopt Opt_KeepRawTokenStream
    <*> const True -- use LINE/COLUMN to update the internal location

supportedLanguagePragmas :: DynFlags -> [String]
supportedLanguagePragmas = supportedLanguagesAndExtensions . platformArchOS . targetPlatform

-- -----------------------------------------------------------------------------
-- Predefined macros, for use in GHC_CPP @PpState@
-- Derived from the GHC source file `ghcversion.h.in`

predefinedMacros :: DynFlags -> Map.Map MacroName MacroDef
predefinedMacros df = Map.fromList
        [
            ( MacroName "__GLASGOW_HASKELL__" Nothing
            , projectVersionInt
            ),
            ( MacroName "__GLASGOW_HASKELL_FULL_VERSION__" Nothing
            , projectVersion
            ),
            ( MacroName  "__GLASGOW_HASKELL_PATCHLEVEL1__" Nothing
            , projectPatchLevel1
            ),
            ( MacroName  "__GLASGOW_HASKELL_PATCHLEVEL2__" Nothing
            , projectPatchLevel2
            )
        ]
  where
    projectVersionInt = fromMaybe "0" (lookup "Project Version Int" (compilerInfo df))
    projectVersion = fromMaybe "0" (lookup "Project version" (compilerInfo df))
    projectPatchLevel1 = fromMaybe "0" (lookup "Project Patch Level1" (compilerInfo df))
    projectPatchLevel2 = fromMaybe "0" (lookup "Project Patch Level2" (compilerInfo df))
