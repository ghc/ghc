{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Distribution.Parsec.Class (
    Parsec(..),
    ParsecParser,
    simpleParsec,
    -- * Warnings
    parsecWarning,
    PWarnType (..),
    -- * Utilities
    parsecToken,
    parsecToken',
    parsecFilePath,
    parsecQuoted,
    parsecMaybeQuoted,
    parsecCommaList,
    parsecOptCommaList,
    parsecStandard,
    parsecUnqualComponentName,
    ) where

import           Data.Functor.Identity       (Identity (..))
import qualified Distribution.Compat.Parsec  as P
import           Distribution.Compat.Prelude
import           Distribution.Parsec.Common  (PWarnType (..), PWarning (..), Position (..))
import           Prelude ()
import qualified Text.Parsec                 as Parsec
import qualified Text.Parsec.Language        as Parsec
import qualified Text.Parsec.Token           as Parsec

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- |
--
-- TODO: implementation details: should be careful about consuming
-- trailing whitespace?
-- Should we always consume it?
class Parsec a where
    parsec :: ParsecParser a

    -- | 'parsec' /could/ consume trailing spaces, this function /must/ consume.
    lexemeParsec :: ParsecParser a
    lexemeParsec = parsec <* P.spaces

type ParsecParser a = forall s. P.Stream s Identity Char => P.Parsec s [PWarning] a

-- | Parse a 'String' with 'lexemeParsec'.
simpleParsec :: Parsec a => String -> Maybe a
simpleParsec
    = either (const Nothing) Just
    . P.runParser (lexemeParsec <* P.eof) [] "<simpleParsec>"

parsecWarning :: PWarnType -> String -> P.Parsec s [PWarning] ()
parsecWarning t w =
    Parsec.modifyState (PWarning t (Position 0 0) w :)

instance Parsec a => Parsec (Identity a) where
    parsec = Identity <$> parsec

instance Parsec Bool where
    parsec = P.munch1 isAlpha >>= postprocess
      where
        postprocess str
            |  str == "True"  = pure True
            |  str == "False" = pure False
            | lstr == "true"  = parsecWarning PWTBoolCase caseWarning *> pure True
            | lstr == "false" = parsecWarning PWTBoolCase caseWarning *> pure False
            | otherwise       = fail $ "Not a boolean: " ++ str
          where
            lstr = map toLower str
            caseWarning =
                "Boolean values are case sensitive, use 'True' or 'False'."

-- | @[^ ,]@
parsecToken :: P.Stream s Identity Char => P.Parsec s [PWarning] String
parsecToken = parsecHaskellString <|> (P.munch1 (\x -> not (isSpace x) && x /= ',')  P.<?> "identifier" )

-- | @[^ ]@
parsecToken' :: P.Stream s Identity Char => P.Parsec s [PWarning] String
parsecToken' = parsecHaskellString <|> (P.munch1 (not . isSpace) P.<?> "token")

parsecFilePath :: P.Stream s Identity Char => P.Parsec s [PWarning] FilePath
parsecFilePath = parsecToken

-- | Parse a benchmark/test-suite types.
parsecStandard
    :: (Parsec ver, P.Stream s Identity Char)
    => (ver -> String -> a)
    -> P.Parsec s [PWarning] a
parsecStandard f = do
    cs   <- some $ P.try (component <* P.char '-')
    ver  <- parsec
    let name = map toLower (intercalate "-" cs)
    return $! f ver name
  where
    component = do
      cs <- P.munch1 isAlphaNum
      if all isDigit cs then fail "all digit component" else return cs
      -- each component must contain an alphabetic character, to avoid
      -- ambiguity in identifiers like foo-1 (the 1 is the version number).

parsecCommaList
    :: P.Stream s Identity Char
    => P.Parsec s [PWarning] a
    -> P.Parsec s [PWarning] [a]
parsecCommaList p = P.sepBy (p <* P.spaces) (P.char ',' *> P.spaces)

parsecOptCommaList
    :: P.Stream s Identity Char
    => P.Parsec s [PWarning] a
    -> P.Parsec s [PWarning] [a]
parsecOptCommaList p = P.sepBy (p <* P.spaces) (P.optional comma)
  where
    comma = P.char ',' *>  P.spaces

-- | Content isn't unquoted
parsecQuoted
     :: P.Stream s Identity Char
     => P.Parsec s [PWarning] a
     -> P.Parsec s [PWarning] a
parsecQuoted = P.between (P.char '"') (P.char '"')

-- | @parsecMaybeQuoted p = 'parsecQuoted' p <|> p@.
parsecMaybeQuoted
     :: P.Stream s Identity Char
     => P.Parsec s [PWarning] a
     -> P.Parsec s [PWarning] a
parsecMaybeQuoted p = parsecQuoted p <|> p

parsecHaskellString :: P.Stream s Identity Char => P.Parsec s [PWarning] String
parsecHaskellString = Parsec.stringLiteral $ Parsec.makeTokenParser Parsec.emptyDef
    { Parsec.commentStart   = "{-"
    , Parsec.commentEnd     = "-}"
    , Parsec.commentLine    = "--"
    , Parsec.nestedComments = True
    , Parsec.identStart     = P.satisfy isAlphaNum
    , Parsec.identLetter    = P.satisfy isAlphaNum <|> P.oneOf "_'"
    , Parsec.opStart        = opl
    , Parsec.opLetter       = opl
    , Parsec.reservedOpNames= []
    , Parsec.reservedNames  = []
    , Parsec.caseSensitive  = True
    }
  where
    opl = P.oneOf ":!#$%&*+./<=>?@\\^|-~"

parsecUnqualComponentName :: P.Stream s Identity Char => P.Parsec s [PWarning] String
parsecUnqualComponentName = intercalate "-" <$> P.sepBy1 component (P.char '-')
  where
    component :: P.Stream s Identity Char => P.Parsec s [PWarning] String
    component = do
      cs <- P.munch1 isAlphaNum
      if all isDigit cs
        then fail "all digits in portion of unqualified component name"
        else return cs
