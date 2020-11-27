module GHC.Parser.Utils
    ( isStmt
    , hasImport
    , isImport
    , isDecl
    )
where

import GHC.Prelude
import GHC.Hs
import GHC.Data.StringBuffer
import GHC.Data.FastString
import GHC.Types.SrcLoc

import qualified GHC.Parser.Lexer as Lexer (P (..), ParseResult(..), unP, initParserState)
import GHC.Parser.Lexer (ParserOpts)
import qualified GHC.Parser       as Parser (parseStmt, parseModule, parseDeclaration, parseImport)


-- | Returns @True@ if passed string is a statement.
isStmt :: ParserOpts -> String -> Bool
isStmt pflags stmt =
  case parseThing Parser.parseStmt pflags stmt of
    Lexer.POk _ _ -> True
    Lexer.PFailed _ -> False

-- | Returns @True@ if passed string has an import declaration.
hasImport :: ParserOpts -> String -> Bool
hasImport pflags stmt =
  case parseThing Parser.parseModule pflags stmt of
    Lexer.POk _ thing -> hasImports thing
    Lexer.PFailed _ -> False
  where
    hasImports = not . null . hsmodImports . unLoc

-- | Returns @True@ if passed string is an import declaration.
isImport :: ParserOpts -> String -> Bool
isImport pflags stmt =
  case parseThing Parser.parseImport pflags stmt of
    Lexer.POk _ _ -> True
    Lexer.PFailed _ -> False

-- | Returns @True@ if passed string is a declaration but __/not a splice/__.
isDecl :: ParserOpts -> String -> Bool
isDecl pflags stmt =
  case parseThing Parser.parseDeclaration pflags stmt of
    Lexer.POk _ thing ->
      case unLoc thing of
        SpliceD _ _ -> False
        _ -> True
    Lexer.PFailed _ -> False

parseThing :: Lexer.P thing -> ParserOpts -> String -> Lexer.ParseResult thing
parseThing parser opts stmt = do
  let buf = stringToStringBuffer stmt
      loc = mkRealSrcLoc (fsLit "<interactive>") 1 1

  Lexer.unP parser (Lexer.initParserState opts buf loc)
