module GHC.Driver.Config.Parser
  ( initParserOpts
  )
where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Utils.Error

import GHC.Parser.Lexer

-- | Extracts the flags needed for parsing
initParserOpts :: DynFlags -> ParserOpts
initParserOpts =
  mkParserOpts
    <$> warningFlags
    <*> extensionFlags
    <*> mkPlainMsgEnvelope
    <*> safeImportsOn
    <*> gopt Opt_Haddock
    <*> gopt Opt_KeepRawTokenStream
    <*> const True -- use LINE/COLUMN to update the internal location

