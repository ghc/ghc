{-# LANGUAGE Rank2Types #-}
module Options.Applicative.Common (
  -- * Option parsers
  --
  -- | A 'Parser' is composed of a list of options. Several kinds of options
  -- are supported:
  --
  --  * Flags: simple no-argument options. When a flag is encountered on the
  --  command line, its value is returned.
  --
  --  * Options: options with an argument. An option can define a /reader/,
  --  which converts its argument from String to the desired value, or throws a
  --  parse error if the argument does not validate correctly.
  --
  --  * Arguments: positional arguments, validated in the same way as option
  --  arguments.
  --
  --  * Commands. A command defines a completely independent sub-parser. When a
  --  command is encountered, the whole command line is passed to the
  --  corresponding parser.
  --
  Parser,
  liftOpt,
  showOption,

  -- * Program descriptions
  --
  -- A 'ParserInfo' describes a command line program, used to generate a help
  -- screen. Two help modes are supported: brief and full. In brief mode, only
  -- an option and argument summary is displayed, while in full mode each
  -- available option and command, including hidden ones, is described.
  --
  -- A basic 'ParserInfo' with default values for fields can be created using
  -- the 'info' function.
  --
  -- A 'ParserPrefs' contains general preferences for all command-line
  -- options, and can be built with the 'prefs' function.
  ParserInfo(..),
  ParserPrefs(..),

  -- * Running parsers
  runParserInfo,
  runParserFully,
  runParserStep,
  runParser,
  evalParser,

  -- * Low-level utilities
  mapParser,
  treeMapParser,
  optionNames
  ) where

import Control.Applicative
import Control.Monad (guard, mzero, msum, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), get, put, runStateT)
import Data.List (isPrefixOf)
import Data.Maybe (maybeToList, isJust, isNothing)
import Prelude

import Options.Applicative.Internal
import Options.Applicative.Types

showOption :: OptName -> String
showOption (OptLong n) = "--" ++ n
showOption (OptShort n) = '-' : [n]

optionNames :: OptReader a -> [OptName]
optionNames (OptReader names _ _) = names
optionNames (FlagReader names _) = names
optionNames _ = []

isOptionPrefix :: OptName -> OptName -> Bool
isOptionPrefix (OptShort x) (OptShort y) = x == y
isOptionPrefix (OptLong x) (OptLong y) = x `isPrefixOf` y
isOptionPrefix _ _ = False

-- | Create a parser composed of a single option.
liftOpt :: Option a -> Parser a
liftOpt = OptP

optMatches :: MonadP m => Bool -> OptReader a -> OptWord -> Maybe (StateT Args m a)
optMatches disambiguate opt (OptWord arg1 val) = case opt of
  OptReader names rdr no_arg_err -> do
    guard $ has_name arg1 names
    Just $ do
      args <- get
      let mb_args = uncons $ maybeToList val ++ args
      let missing_arg = missingArgP (no_arg_err $ showOption arg1) (crCompleter rdr)
      (arg', args') <- maybe (lift missing_arg) return mb_args
      put args'
      lift $ runReadM (withReadM (errorFor arg1) (crReader rdr)) arg'

  FlagReader names x -> do
    guard $ has_name arg1 names
    -- #242 Flags/switches succeed incorrectly when given an argument.
    -- We'll not match a long option for a flag if there's a word attached.
    -- This was revealing an implementation detail as
    -- `--foo=val` was being parsed as `--foo -val`, which is gibberish.
    guard $ isShortName arg1 || isNothing val
    Just $ do
      args <- get
      let val' = ('-' :) <$> val
      put $ maybeToList val' ++ args
      return x
  _ -> Nothing
  where
    errorFor name msg = "option " ++ showOption name ++ ": " ++ msg

    has_name a
      | disambiguate = any (isOptionPrefix a)
      | otherwise = elem a

isArg :: OptReader a -> Bool
isArg (ArgReader _) = True
isArg _ = False

data OptWord = OptWord OptName (Maybe String)

parseWord :: String -> Maybe OptWord
parseWord ('-' : '-' : w) = Just $ let
  (opt, arg) = case span (/= '=') w of
    (_, "") -> (w, Nothing)
    (w', _ : rest) -> (w', Just rest)
  in OptWord (OptLong opt) arg
parseWord ('-' : w) = case w of
  [] -> Nothing
  (a : rest) -> Just $ let
    arg = rest <$ guard (not (null rest))
    in OptWord (OptShort a) arg
parseWord _ = Nothing

searchParser :: Monad m
             => (forall r . Option r -> NondetT m (Parser r))
             -> Parser a -> NondetT m (Parser a)
searchParser _ (NilP _) = mzero
searchParser f (OptP opt) = f opt
searchParser f (MultP p1 p2) = foldr1 (<!>)
  [ do p1' <- searchParser f p1
       return (p1' <*> p2)
  , do p2' <- searchParser f p2
       return (p1 <*> p2') ]
searchParser f (AltP p1 p2) = msum
  [ searchParser f p1
  , searchParser f p2 ]
searchParser f (BindP p k) = msum
  [ do p' <- searchParser f p
       return $ BindP p' k
  , case evalParser p of
      Nothing -> mzero
      Just aa -> searchParser f (k aa) ]

searchOpt :: MonadP m => ParserPrefs -> OptWord -> Parser a
          -> NondetT (StateT Args m) (Parser a)
searchOpt pprefs w = searchParser $ \opt -> do
  let disambiguate = prefDisambiguate pprefs
                  && optVisibility opt > Internal
  case optMatches disambiguate (optMain opt) w of
    Just matcher -> lift $ fmap pure matcher
    Nothing -> mzero

searchArg :: MonadP m => ParserPrefs -> String -> Parser a
          -> NondetT (StateT Args m) (Parser a)
searchArg prefs arg =
  searchParser $ \opt -> do
    when (isArg (optMain opt)) cut
    case optMain opt of
      CmdReader _ _ f ->
        case (f arg, prefBacktrack prefs) of
          (Just subp, NoBacktrack) -> lift $ do
            args <- get <* put []
            fmap pure . lift $ enterContext arg subp *> runParserInfo subp args <* exitContext

          (Just subp, Backtrack) -> fmap pure . lift . StateT $ \args ->
            enterContext arg subp *> runParser (infoPolicy subp) CmdStart (infoParser subp) args <* exitContext

          (Just subp, SubparserInline) -> lift $ do
            lift $ enterContext arg subp
            return $ infoParser subp

          (Nothing, _)  -> mzero
      ArgReader rdr ->
        fmap pure . lift . lift $ runReadM (crReader rdr) arg
      _ -> mzero

stepParser :: MonadP m => ParserPrefs -> ArgPolicy -> String
           -> Parser a -> NondetT (StateT Args m) (Parser a)
stepParser pprefs AllPositionals arg p =
  searchArg pprefs arg p
stepParser pprefs ForwardOptions arg p = case parseWord arg of
  Just w -> searchOpt pprefs w p <|> searchArg pprefs arg p
  Nothing -> searchArg pprefs arg p
stepParser pprefs _ arg p = case parseWord arg of
  Just w -> searchOpt pprefs w p
  Nothing -> searchArg pprefs arg p


-- | Apply a 'Parser' to a command line, and return a result and leftover
-- arguments.  This function returns an error if any parsing error occurs, or
-- if any options are missing and don't have a default value.
runParser :: MonadP m => ArgPolicy -> IsCmdStart -> Parser a -> Args -> m (a, Args)
runParser policy _ p ("--" : argt) | policy /= AllPositionals
                                   = runParser AllPositionals CmdCont p argt
runParser policy isCmdStart p args = case args of
  [] -> exitP isCmdStart policy p result
  (arg : argt) -> do
    (mp', args') <- do_step arg argt
    case mp' of
      Nothing -> hoistMaybe result <|> parseError arg p
      Just p' -> runParser (newPolicy arg) CmdCont p' args'
  where
    result =
      (,) <$> evalParser p <*> pure args
    do_step =
      runParserStep policy p

    newPolicy a = case policy of
      NoIntersperse -> if isJust (parseWord a) then NoIntersperse else AllPositionals
      x             -> x

runParserStep :: MonadP m => ArgPolicy -> Parser a -> String -> Args -> m (Maybe (Parser a), Args)
runParserStep policy p arg args = do
  prefs <- getPrefs
  flip runStateT args
    $ disamb (not (prefDisambiguate prefs))
    $ stepParser prefs policy arg p

parseError :: MonadP m => String -> Parser x -> m a
parseError arg = errorP . UnexpectedError arg . SomeParser

runParserInfo :: MonadP m => ParserInfo a -> Args -> m a
runParserInfo i = runParserFully (infoPolicy i) (infoParser i)

runParserFully :: MonadP m => ArgPolicy -> Parser a -> Args -> m a
runParserFully policy p args = do
  (r, args') <- runParser policy CmdStart p args
  case args' of
    []  -> return r
    a:_ -> parseError a (pure ())

-- | The default value of a 'Parser'.  This function returns an error if any of
-- the options don't have a default value.
evalParser :: Parser a -> Maybe a
evalParser (NilP r) = r
evalParser (OptP _) = Nothing
evalParser (MultP p1 p2) = evalParser p1 <*> evalParser p2
evalParser (AltP p1 p2) = evalParser p1 <|> evalParser p2
evalParser (BindP p k) = evalParser p >>= evalParser . k

-- | Map a polymorphic function over all the options of a parser, and collect
-- the results in a list.
mapParser :: (forall x. ArgumentReachability -> Option x -> b)
          -> Parser a -> [b]
mapParser f = flatten . treeMapParser f
  where
    flatten (Leaf x) = [x]
    flatten (MultNode xs) = xs >>= flatten
    flatten (AltNode _ xs) = xs >>= flatten
    flatten (BindNode x) = flatten x

-- | Like 'mapParser', but collect the results in a tree structure.
treeMapParser :: (forall x. ArgumentReachability -> Option x -> b)
          -> Parser a
          -> OptTree b
treeMapParser g = simplify . go False g
  where
    has_default :: Parser a -> Bool
    has_default p = isJust (evalParser p)

    go :: Bool
       -> (forall x. ArgumentReachability -> Option x -> b)
       -> Parser a
       -> OptTree b
    go _ _ (NilP _) = MultNode []
    go r f (OptP opt)
      | optVisibility opt > Internal
      = Leaf (f (ArgumentReachability r) opt)
      | otherwise
      = MultNode []
    go r f (MultP p1 p2) =
      MultNode [go r f p1, go r' f p2]
      where r' = r || hasArg p1
    go r f (AltP p1 p2) =
      AltNode altNodeType [go r f p1, go r f p2]
      where
        -- The 'AltNode' indicates if one of the branches has a default.
        -- This is used for rendering brackets, as well as filtering
        -- out optional arguments when generating the "missing:" text.
        altNodeType =
          if has_default p1 || has_default p2
            then MarkDefault
            else NoDefault

    go r f (BindP p k) =
      let go' = go r f p
      in case evalParser p of
        Nothing -> BindNode go'
        Just aa -> BindNode (MultNode [ go', go r f (k aa) ])

    hasArg :: Parser a -> Bool
    hasArg (NilP _) = False
    hasArg (OptP p) = (isArg . optMain) p
    hasArg (MultP p1 p2) = hasArg p1 || hasArg p2
    hasArg (AltP p1 p2) = hasArg p1 || hasArg p2
    hasArg (BindP p _) = hasArg p

simplify :: OptTree a -> OptTree a
simplify (Leaf x) = Leaf x
simplify (MultNode xs) =
  case concatMap (remove_mult . simplify) xs of
    [x] -> x
    xs' -> MultNode xs'
  where
    remove_mult (MultNode ts) = ts
    remove_mult t = [t]
simplify (AltNode b xs) =
  AltNode b (concatMap (remove_alt . simplify) xs)
  where
    remove_alt (AltNode _ ts) = ts
    remove_alt (MultNode []) = []
    remove_alt t = [t]
simplify (BindNode x) =
  BindNode $ simplify x
