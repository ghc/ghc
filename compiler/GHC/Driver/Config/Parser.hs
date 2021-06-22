module GHC.Driver.Config.Parser
  ( initParserOpts
  )
where

import GHC.Prelude
import GHC.Platform

import GHC.Driver.Session
import GHC.Driver.Config.Diagnostic

import GHC.Parser.Lexer

-- | Extracts the flags needed for parsing
initParserOpts :: DynFlags -> ParserOpts
initParserOpts =
  mkParserOpts
    <$> extensionFlags
    <*> initDiagOpts
    <*> (supportedLanguagesAndExtensions . platformArchOS . targetPlatform)
    <*> safeImportsOn
    <*> gopt Opt_Haddock
    <*> gopt Opt_KeepRawTokenStream
    <*> const True -- use LINE/COLUMN to update the internal location

