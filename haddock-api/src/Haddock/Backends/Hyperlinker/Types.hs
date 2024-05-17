{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Haddock.Backends.Hyperlinker.Types where

import qualified GHC

import Data.ByteString (ByteString)

import Data.Map (Map)

data Token = Token
  { tkType :: TokenType
  , tkValue :: ByteString
  -- ^ UTF-8 encoded
  , tkSpan :: {-# UNPACK #-} !Span
  }
  deriving (Show)

pattern BacktickTok, OpenParenTok, CloseParenTok :: Span -> Token
pattern BacktickTok sp = Token TkSpecial "`" sp
pattern OpenParenTok sp = Token TkSpecial "(" sp
pattern CloseParenTok sp = Token TkSpecial ")" sp

type Position = GHC.RealSrcLoc
type Span = GHC.RealSrcSpan

data TokenType
  = TkIdentifier
  | TkKeyword
  | TkString
  | TkChar
  | TkNumber
  | TkOperator
  | TkGlyph
  | TkSpecial
  | TkSpace
  | TkComment
  | TkCpp
  | TkPragma
  | TkUnknown
  deriving (Show, Eq)

-- | Path for making cross-package hyperlinks in generated sources.
--
-- Used in 'SrcMap' to determine whether module originates in current package
-- or in an external package.
data SrcPath
  = SrcExternal FilePath
  | SrcLocal

-- | Mapping from modules to cross-package source paths.
type SrcMaps = (Map GHC.Module SrcPath, Map GHC.ModuleName SrcPath)
