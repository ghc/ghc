{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module provides a 'FieldGrammarParser', one way to parse
-- @.cabal@ -like files.
--
-- Fields can be specified multiple times in the .cabal files.  The order of
-- such entries is important, but the mutual ordering of different fields is
-- not.Also conditional sections are considered after non-conditional data.
-- The example of this silent-commutation quirk is the fact that
--
-- @
-- buildable: True
-- if os(linux)
--   buildable: False
-- @
--
-- and
--
-- @
-- if os(linux)
--   buildable: False
-- buildable: True
-- @
--
-- behave the same! This is the limitation of 'GeneralPackageDescription'
-- structure.
--
-- So we transform the list of fields @['Field' ann]@ into
-- a map of grouped ordinary fields and a list of lists of sections:
-- @'Fields' ann = 'Map' 'FieldName' ['NamelessField' ann]@ and @[['Section' ann]]@.
--
-- We need list of list of sections, because we need to distinguish situations
-- where there are fields in between. For example
--
-- @
-- if flag(bytestring-lt-0_10_4)
--   build-depends: bytestring < 0.10.4
--
-- default-language: Haskell2020
--
-- else
--   build-depends: bytestring >= 0.10.4
--
-- @
--
-- is obviously invalid specification.
--
-- We can parse 'Fields' like we parse @aeson@ objects, yet we use
-- slighly higher-level API, so we can process unspecified fields,
-- to report unknown fields and save custom @x-fields@.
--
module Distribution.FieldGrammar.Parsec (
    ParsecFieldGrammar,
    parseFieldGrammar,
    fieldGrammarKnownFieldList,
    -- * Auxiliary
    Fields,
    NamelessField (..),
    Section (..),
    runFieldParser,
    runFieldParser',
    )  where

import qualified Data.ByteString                as BS
import           Data.List                      (dropWhileEnd)
import           Data.Ord                       (comparing)
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import qualified Distribution.Compat.Map.Strict as Map
import           Distribution.Compat.Prelude
import           Distribution.Compat.Newtype
import           Distribution.Simple.Utils      (fromUTF8BS)
import           Prelude ()
import qualified Text.Parsec                    as P
import qualified Text.Parsec.Error              as P

import Distribution.FieldGrammar.Class
import Distribution.Parsec.Class
import Distribution.Parsec.Common
import Distribution.Parsec.Field
import Distribution.Parsec.ParseResult

-------------------------------------------------------------------------------
-- Auxiliary types
-------------------------------------------------------------------------------

type Fields ann = Map FieldName [NamelessField ann]

-- | Single field, without name, but with its annotation.
data NamelessField ann = MkNamelessField !ann [FieldLine ann]
  deriving (Eq, Show, Functor)

-- | The 'Section' constructor of 'Field'.
data Section ann = MkSection !(Name ann) [SectionArg ann] [Field ann]
  deriving (Eq, Show, Functor)

-------------------------------------------------------------------------------
-- ParsecFieldGrammar
-------------------------------------------------------------------------------

data ParsecFieldGrammar s a = ParsecFG
    { fieldGrammarKnownFields   :: !(Set FieldName)
    , fieldGrammarKnownPrefixes :: !(Set FieldName)
    , fieldGrammarParser        :: !(Fields Position -> ParseResult a)
    }
  deriving (Functor)

parseFieldGrammar :: Fields Position -> ParsecFieldGrammar s a -> ParseResult a
parseFieldGrammar fields grammar = do
    for_ (Map.toList (Map.filterWithKey isUnknownField fields)) $ \(name, nfields) ->
        for_ nfields $ \(MkNamelessField pos _) ->
            parseWarning pos PWTUnknownField $ "Unknown field: " ++ show name
            -- TODO: fields allowed in this section

    -- parse
    fieldGrammarParser grammar fields

  where
    isUnknownField k _ = not $
        k `Set.member` fieldGrammarKnownFields grammar
        || any (`BS.isPrefixOf` k) (fieldGrammarKnownPrefixes grammar)

fieldGrammarKnownFieldList :: ParsecFieldGrammar s a -> [FieldName]
fieldGrammarKnownFieldList = Set.toList . fieldGrammarKnownFields

instance Applicative (ParsecFieldGrammar s) where
    pure x = ParsecFG mempty mempty (\_ ->  pure x)
    {-# INLINE pure  #-}

    ParsecFG f f' f'' <*> ParsecFG x x' x'' = ParsecFG
        (mappend f x)
        (mappend f' x')
        (\fields -> f'' fields <*> x'' fields)
    {-# INLINE (<*>) #-}

instance FieldGrammar ParsecFieldGrammar where
    blurFieldGrammar _ (ParsecFG s s' parser) = ParsecFG s s' parser

    uniqueFieldAla fn _pack _extract = ParsecFG (Set.singleton fn) Set.empty parser
      where
        parser fields = case Map.lookup fn fields of
            Nothing -> parseFatalFailure zeroPos $ show fn ++ " field missing:"
            Just [] -> parseFatalFailure zeroPos $ show fn ++ " field foo"
            Just [x] -> parseOne x
            -- TODO: parse all
            -- TODO: warn about duplicate fields?
            Just xs-> parseOne (last xs)

        parseOne (MkNamelessField pos fls) =
            unpack' _pack <$> runFieldParser pos parsec fls

    booleanFieldDef fn _extract def = ParsecFG (Set.singleton fn) Set.empty parser
      where
        parser :: Fields Position -> ParseResult Bool
        parser fields = case Map.lookup fn fields of
            Nothing  -> pure def
            Just []  -> pure def
            Just [x] -> parseOne x
            -- TODO: parse all
            -- TODO: warn about duplicate optional fields?
            Just xs  -> parseOne (last xs)

        parseOne (MkNamelessField pos fls) = runFieldParser pos parsec fls

    optionalFieldAla fn _pack _extract = ParsecFG (Set.singleton fn) Set.empty parser
      where
        parser fields = case Map.lookup fn fields of
            Nothing  -> pure Nothing
            Just []  -> pure Nothing
            Just [x] -> parseOne x
            -- TODO: parse all!
            Just xs  -> parseOne (last xs) -- TODO: warn about duplicate optional fields?

        parseOne (MkNamelessField pos fls)
            | null fls  = pure Nothing
            | otherwise = Just . (unpack' _pack) <$> runFieldParser pos parsec fls

    monoidalFieldAla fn _pack _extract = ParsecFG (Set.singleton fn) Set.empty parser
      where
        parser fields = case Map.lookup fn fields of
            Nothing -> pure mempty
            Just xs -> foldMap (unpack' _pack) <$> traverse parseOne xs

        parseOne (MkNamelessField pos fls) = runFieldParser pos parsec fls

    prefixedFields fnPfx _extract = ParsecFG mempty (Set.singleton fnPfx) (pure . parser)
      where
        parser :: Fields Position -> [(String, String)]
        parser values = reorder $ concatMap convert $ filter match $ Map.toList values

        match (fn, _) = fnPfx `BS.isPrefixOf` fn
        convert (fn, fields) =
            -- TODO: warn about invalid UTF8
            [ (pos, (fromUTF8BS fn, trim $ fromUTF8BS $ fieldlinesToBS fls))
            | MkNamelessField pos fls <- fields
            ]
        -- hack: recover the order of prefixed fields
        reorder = map snd . sortBy (comparing fst)
        trim :: String -> String
        trim = dropWhile isSpace . dropWhileEnd isSpace

    availableSince _ = id

    deprecatedSince (_ : _) _ grammar = grammar -- pass on non-empty version
    deprecatedSince _ msg (ParsecFG names prefixes parser) = ParsecFG names prefixes parser'
      where
        parser' values = do
            let deprecatedFields = Map.intersection values $ Map.fromSet (const ()) names
            for_ (Map.toList deprecatedFields) $ \(name, fields) ->
                for_ fields $ \(MkNamelessField pos _) ->
                    parseWarning pos PWTDeprecatedField $
                        "The field " <> show name <> " is deprecated. " ++ msg

            parser values

    knownField fn = ParsecFG (Set.singleton fn) Set.empty (\_ -> pure ())

    hiddenField = id

-------------------------------------------------------------------------------
-- Parsec
-------------------------------------------------------------------------------

runFieldParser' :: Position -> FieldParser a -> String -> ParseResult a
runFieldParser' (Position row col) p str = case P.runParser p' [] "<field>" str of
    Right (pok, ws) -> do
        -- TODO: map pos
        traverse_ (\(PWarning t pos w) -> parseWarning pos t w) ws
        pure pok
    Left err        -> do
        let ppos = P.errorPos err
        -- Positions start from 1:1, not 0:0
        let epos = Position (row - 1 + P.sourceLine ppos) (col - 1 + P.sourceColumn ppos)
        let msg = P.showErrorMessages
                "or" "unknown parse error" "expecting" "unexpected" "end of input"
                (P.errorMessages err)

        parseFatalFailure epos $ msg ++ ": " ++ show str
  where
    p' = (,) <$ P.spaces <*> p <* P.spaces <* P.eof <*> P.getState

runFieldParser :: Position -> FieldParser a -> [FieldLine Position] -> ParseResult a
runFieldParser pp p ls = runFieldParser' pos p =<< fieldlinesToString pos ls
  where
    -- TODO: make per line lookup
    pos = case ls of
        []                     -> pp
        (FieldLine pos' _ : _) -> pos'

fieldlinesToBS :: [FieldLine ann] -> BS.ByteString
fieldlinesToBS = BS.intercalate "\n" . map (\(FieldLine _ bs) -> bs)

-- TODO: Take position  from FieldLine
-- TODO: Take field name
fieldlinesToString :: Position -> [FieldLine ann] -> ParseResult String
fieldlinesToString pos fls =
    let str = intercalate "\n" . map (\(FieldLine _ bs') -> fromUTF8BS bs') $ fls
    in if '\xfffd' `elem` str
        then str <$ parseWarning pos PWTUTF "Invalid UTF8 encoding"
        else pure str
