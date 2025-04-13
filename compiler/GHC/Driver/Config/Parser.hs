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
import qualified GHC.Parser.PreProcess.ParserM as PM
import GHC.Parser.PreProcess.State ( MacroDefines, CppDirective (..))
import GHC.Parser.PreProcess.ParsePP (parseDirective)
import GHC.Utils.Panic.Plain (panic)

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

predefinedMacros :: DynFlags -> MacroDefines
predefinedMacros df = Map.fromList
        [
            ( "__GHCVERSION_H__" , Map.empty
            ),
            ( "__GLASGOW_HASKELL__"
            , Map.singleton Nothing (Nothing, [PM.TInteger projectVersionInt])
            ),
            ( "__GLASGOW_HASKELL_FULL_VERSION__"
            , Map.singleton Nothing (Nothing, [PM.TOther projectVersion])
            ),
            ( "__GLASGOW_HASKELL_PATCHLEVEL1__"
            , Map.singleton Nothing (Nothing, [PM.TOther projectPatchLevel1])
            ),
            ( "__GLASGOW_HASKELL_PATCHLEVEL2__"
            , Map.singleton Nothing (Nothing, [PM.TOther projectPatchLevel2])),
            min_version_macro,

            -- TODO: What is the appropriate indicator that GHC_CPP is active?
            ( "GHC_CPP", Map.empty)
        ]
  where
    projectVersionInt = fromMaybe "0" (lookup "Project Version Int" (compilerInfo df))
    projectVersion = fromMaybe "0" (lookup "Project version" (compilerInfo df))
    projectPatchLevel1 = fromMaybe "0" (lookup "Project Patch Level1" (compilerInfo df))
    projectPatchLevel2 = fromMaybe "0" (lookup "Project Patch Level2" (compilerInfo df))
    min_version_macro = case parseDirective (concat min_version_macro_src) of
      Right (CppDefine name args def) -> (name, Map.singleton (Just 4) (args, def))
      _ -> panic  "min_version_macro"

    min_version_macro_src =
      [ "#define MIN_VERSION_GLASGOW_HASKELL(ma,mi,pl1,pl2) ( "
      , "   ((ma)*100+(mi)) <  " ++ projectVersionInt  ++ " || "
      , "   ((ma)*100+(mi)) == " ++ projectVersionInt  ++ "    "
      , "          && (pl1) <  " ++ projectPatchLevel1 ++ " || "
      , "   ((ma)*100+(mi)) == " ++ projectVersionInt  ++ " "
      , "          && (pl1) == " ++ projectPatchLevel1 ++ " "
      , "          && (pl2) <= " ++ projectPatchLevel2 ++ " )"
      ]
