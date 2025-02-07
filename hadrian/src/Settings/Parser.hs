{-# LANGUAGE FlexibleContexts #-}
-- | Utilities for implementing key-value settings, as described in Note [Hadrian settings]
module Settings.Parser where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State as State
import Data.Either
import Data.List

import qualified Text.Parsec as Parsec

-- * Raw parsing of @key = value@ or @key += value@ expressions

-- | A 'Key' is parsed from a dot-separated list of words.
type Key = [String]

-- | A 'Val'ue is any 'String'.
type Val = String

-- | 'Equal' when overriding the entire computation of a setting with some
--   fresh values, 'PlusEqual' when extending it.
data Op = Equal | PlusEqual
  deriving (Eq, Ord, Show)

-- | A 'KeyVal' represents an expression @foo.bar.baz [+]= v@.
data KeyVal = KeyVal Key Op Val
  deriving (Eq, Ord, Show)

-- | Pretty-print 'KeyVal's.
ppKeyVals :: [KeyVal] -> String
ppKeyVals = unlines . map ppKeyVal

-- | Pretty-print a 'KeyVal'.
ppKeyVal :: KeyVal -> String
ppKeyVal (KeyVal k op v) =
  intercalate "." k ++ " " ++ opstr ++ " " ++ v

  where opstr = case op of
          Equal     ->  "="
          PlusEqual -> "+="

-- | Remove any string that can be parsed as a 'KeyVal' from the
--   given list.
removeKVs :: [String] -> [String]
removeKVs xs = fst (partitionKVs xs)

-- | Try to parse all strings of the given list as 'KeyVal's and keep
--   only the successful parses.
parseJustKVs :: [String] -> [KeyVal]
parseJustKVs xs = snd (partitionKVs xs)

-- | Try to parse all strings from the given list as 'KeyVal's and return
--   the ones for which parsing fails in the first component of the pair,
--   and the successful parses in the second component of the pair.
partitionKVs :: [String] -> ([String], [KeyVal])
partitionKVs xs = partitionEithers $
  map (\x -> either (const $ Left x) Right $ parseKV x) xs

-- | Try to parse all strings from the input list as 'KeyVal's.
parseKVs :: [String] -> [Either Parsec.ParseError KeyVal]
parseKVs = map parseKV

-- | Try to parse the given string as a 'KeyVal'.
parseKV :: String -> Either Parsec.ParseError KeyVal
parseKV = Parsec.parse parseKeyVal "<string list>"

-- | This implements a parser that supports @key = val@, @key = "val"@,
--   @key += val@, @key += "val"@ style syntax, where there can be 0 or more
--   spaces between the key and the operator, and the operator and the value.
parseKeyVal :: Parsec.Parsec String () KeyVal
parseKeyVal = do
  k <- parseKey
  skipSpaces
  op <- parseOp
  skipSpaces
  v <- parseValue
  return (KeyVal k op v)

  where skipSpaces = Parsec.optional (Parsec.many1 (Parsec.oneOf " \t"))

-- | Parse a dot-separated list of alpha-numerical words that can contain
--   dashes, just not at the beginning.
parseKey :: Parsec.Parsec String () Key
parseKey =
  Parsec.sepBy1 (starOr $ liftA2 (:) Parsec.alphaNum $
                  Parsec.many (Parsec.alphaNum <|> Parsec.char '-')
                )
                (Parsec.char '.')

  where starOr :: Parsec.Parsec String () String -> Parsec.Parsec String () String
        starOr p = ((\x -> [x]) <$> Parsec.char '*') <|> p

-- | Parse @=@ or @+=@.
parseOp :: Parsec.Parsec String () Op
parseOp = Parsec.choice
  [ Parsec.char '=' *> pure Equal
  , Parsec.string "+=" *> pure PlusEqual
  ]

-- | Parse @some string@ or @\"some string\"@.
parseValue :: Parsec.Parsec String () Val
parseValue = Parsec.optional (Parsec.char '\"') >> Parsec.manyTill Parsec.anyChar ((Parsec.char '\"' >> pure ()) <|> Parsec.eof)

-- * Expressing settings

-- | The current key component must match the given string.
str :: Match f => String -> f ()
str = matchString

-- | Like 'str', but returns the second argument insead of @()@.
--
-- > val s a = str s *> pure a
val :: Match f => String -> a -> f a
val s a = str s *> pure a

-- | Try and match one of the given "matchers".
--
-- > oneOf [str "hello", str "hi"] -- matches "hello" or "hi"
oneOf :: Match f => [f a] -> f a
oneOf = matchOneOf

-- | Try and match one of the given strings, returning the corresponding
--   value (the @a@) when the input matches.
choose :: Match f => [(String, a)] -> f a
choose xs = oneOf $ map (uncurry val) xs

-- | Try and match one of the given strings, or @*@, and return
--   the corresponding value (@One someValue@ or @Wildcard@ respectively).
wild :: Match f => [(String, a)] -> f (Wildcard a)
wild xs = choose $ ("*", Wildcard) : map (fmap One) xs

-- * Wildcards (@*@) in settings

-- | A @'Wildcard' a@ is either 'Wildcard' or @One x@ where @x :: a@.
data Wildcard a = Wildcard | One a
  deriving (Eq, Ord, Show)

-- | Elimination rule for 'Wildcard'. The first argument is returned
--   when the input is 'Wildcard', and when it's not the second argument
--   is applied to the value wrapped behind 'One'.
wildcard :: b -> (a -> b) -> Wildcard a -> b
wildcard z f x = case x of
  Wildcard -> z
  One a    -> f a

-- * 'Match' class, to interpret settings in various ways

-- 'matchOneOf' is similar in spirit to Alternative's '<|>',
-- but we don't really have an identity ('empty').
--
-- 'matchString' on the other hand is just a handy primitive.
--
-- Selective functors may be relevant here...?

-- | Equip the 'Applicative' class with a primitive to match a known string,
--   and another to try and match a bunch of "alternatives", returning
--   the first one that succeeds.
class Applicative f => Match f where
  matchString :: String -> f ()
  matchOneOf :: [f a] -> f a

-- * 'SettingsM' interpretation

type SettingError = String

type SettingsM = StateT Key (Either SettingError)

-- | Runs the 'SettingsM' computation, returning the value at the leaf
--   when the given 'Key' matches exactly at least one setting, erroring
--   out when it is too long or just doesn't match.
runSettingsM :: Key -> SettingsM a -> Either SettingError a
runSettingsM k m = case runStateT m k of
  Left  err     -> Left err
  Right (a, []) -> return a
  Right (_, xs) -> throwError $ "suffix " ++ show xs ++ " not found in settings"

-- | Stateful manipulation of the remaining key components,
--   with errors when strings don't match.
instance Match SettingsM where
  matchString = matchStringSettingsM
  matchOneOf = matchOneOfSettingsM

matchStringSettingsM :: String -> SettingsM ()
matchStringSettingsM s = do
  ks <- State.get
  case ks of
    []            -> throwError $ "expected " ++ show s ++ ", got nothing"
    k:_
      | k == s    -> State.modify (drop 1)
      | otherwise -> throwError $ "expected " ++ show s ++ ", got " ++ show k

matchOneOfSettingsM :: [SettingsM a] -> SettingsM a
matchOneOfSettingsM acts = StateT $ \k -> do
  firstMatch $ map (($ k) . State.runStateT) acts

  where firstMatch
          :: [Either SettingError (a, Key)]
          -> Either SettingError (a, Key)
        firstMatch []              = throwError "matchOneOf: no match"
        firstMatch (Left _ : xs)   = firstMatch xs
        firstMatch (Right res : _) = return res

-- * Completion interpretation

-- | A tree with values at the leaves ('Pure'), but where we can
--   have "linear links" with strings attached.
--
--   - @'Known' s t@ nodes are used to represent matching against
--     known strings;
--   - @'Branch' ts@ nodes are used to represent matching against a list
--     of "matchers";
--   - @'Pure' a@ nodes are used to attach values at the leaves, and help
--     provide an 'Applicative' interface.
data CompletionTree a
  = Known String (CompletionTree a)
  | Branch [CompletionTree a]
  | Pure a
  deriving (Eq, Show)

-- | Traverses 'Known' and 'Branch' nodes, only applying the
--   function to values at the leaves, wrapped with 'Pure'.
instance Functor CompletionTree where
  fmap f (Known s t) = Known s (fmap f t)
  fmap f (Branch ts) = Branch (map (fmap f) ts)
  fmap f (Pure a)    = Pure (f a)

-- | 'pure' is 'Pure', '<*>' distributes the choices.
instance Applicative CompletionTree where
  pure = Pure

  Pure f <*> Pure x    = Pure (f x)
  Pure f <*> Known s t = Known s (fmap f t)
  Pure f <*> Branch xs = Branch (map (fmap f) xs)
  Known s t <*> t'  = Known s (t <*> t')
  Branch ts <*> t'  = Branch (map (<*> t') ts)

-- | 'matchString' gets mapped to 'Known', 'matchOneOf' to 'Branch'.
instance Match CompletionTree where
  matchString s = Known s (Pure ())
  matchOneOf xs = Branch xs

-- | Enumerate all the keys a completion tree represents, with the corresponding
--   leave values.
--
--   > enumerate (val "hello" 1)) -- [(1, ["hello"])]
enumerate :: CompletionTree a -> [(a, Key)]
enumerate = go []

  where go ks (Known s t) = go (s:ks) t
        go ks (Branch xs) = concatMap (go ks) xs
        go ks (Pure a) = [(a, reverse ks)]

-- | Enumerate all the valid completions for the given input (a partially-written
--   setting key).
--
--   > complete ["hel"] (val 1 "hello")
--   >   -- returns [(1, ["hello"])]
--   > complete ["foo"] (str "foo" *> oneOf [val "hello" 1, val "world" 2])
--   >   -- returns [(1, ["foo", "hello"]), (2, ["foo", "world"])]
complete :: [String] -> CompletionTree a -> [(a, Key)]
complete [] t = enumerate t
complete (k:ks) t = case t of
  Known s t'
    | k == s -> map (fmap (s:)) (complete ks t')
    | (k `isPrefixOf` s) && null ks -> map (fmap (s:)) (enumerate t')
      -- TODO: use an Either to indicate suggestions about
      -- typos somewhere in the middle (not for the final component)
      -- (e.g "You wrote stage1.ghc-bi.ghc.hs.opts but probably
      -- meant stage1.ghc-bin.ghc.hs.opts") ?
    | otherwise -> []
  Branch ts -> concatMap (complete (k:ks)) ts
  Pure a -> return (a, [])
