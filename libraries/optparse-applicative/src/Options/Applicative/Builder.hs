{-# LANGUAGE CPP #-}

module Options.Applicative.Builder (
  -- * Parser builders
  --
  -- | This module contains utility functions and combinators to create parsers
  -- for individual options.
  --
  -- Each parser builder takes an option modifier. A modifier can be created by
  -- composing the basic modifiers provided by this module using the 'Monoid'
  -- operations 'mempty' and 'mappend', or their aliases 'idm' and '<>'.
  --
  -- For example:
  --
  -- > out = strOption
  -- >     ( long "output"
  -- >    <> short 'o'
  -- >    <> metavar "FILENAME" )
  --
  -- creates a parser for an option called \"output\".
  subparser,
  strArgument,
  argument,
  flag,
  flag',
  switch,
  abortOption,
  infoOption,
  strOption,
  option,

  -- * Modifiers
  short,
  long,
  help,
  helpDoc,
  value,
  showDefaultWith,
  showDefault,
  metavar,
  noArgError,
  ParseError(..),
  hidden,
  internal,
  style,
  command,
  commandGroup,
  completeWith,
  action,
  completer,
  idm,
  mappend,

  -- * Readers
  --
  -- | A collection of basic 'Option' readers.
  auto,
  str,
  maybeReader,
  eitherReader,
  disabled,
  readerAbort,
  readerError,

  -- * Builder for 'ParserInfo'
  InfoMod,
  fullDesc,
  briefDesc,
  header,
  headerDoc,
  footer,
  footerDoc,
  progDesc,
  progDescDoc,
  failureCode,
  noIntersperse,
  forwardOptions,
  allPositional,
  info,

  -- * Builder for 'ParserPrefs'
  PrefsMod,
  multiSuffix,
  disambiguate,
  showHelpOnError,
  showHelpOnEmpty,
  noBacktrack,
  subparserInline,
  columns,
  helpLongEquals,
  helpShowGlobals,
  prefs,
  defaultPrefs,

  -- * Types
  Mod,
  ReadM,
  OptionFields,
  FlagFields,
  ArgumentFields,
  CommandFields,

  HasName,
  HasCompleter,
  HasValue,
  HasMetavar
  ) where

import Control.Applicative
#if __GLASGOW_HASKELL__ <= 802
import Data.Semigroup hiding (option)
#endif
import Data.String (fromString, IsString)

import Options.Applicative.Builder.Completer
import Options.Applicative.Builder.Internal
import Options.Applicative.Common
import Options.Applicative.Types
import Options.Applicative.Help.Pretty
import Options.Applicative.Help.Chunk

-- Readers --

-- | 'Option' reader based on the 'Read' type class.
auto :: Read a => ReadM a
auto = eitherReader $ \arg -> case reads arg of
  [(r, "")] -> return r
  _         -> Left $ "cannot parse value `" ++ arg ++ "'"

-- | String 'Option' reader.
--
--   Polymorphic over the `IsString` type class since 0.14.
str :: IsString s => ReadM s
str = fromString <$> readerAsk

-- | Convert a function producing an 'Either' into a reader.
--
-- As an example, one can create a ReadM from an attoparsec Parser
-- easily with
--
-- > import qualified Data.Attoparsec.Text as A
-- > import qualified Data.Text as T
-- > attoparsecReader :: A.Parser a -> ReadM a
-- > attoparsecReader p = eitherReader (A.parseOnly p . T.pack)
eitherReader :: (String -> Either String a) -> ReadM a
eitherReader f = readerAsk >>= either readerError return . f

-- | Convert a function producing a 'Maybe' into a reader.
maybeReader :: (String -> Maybe a) -> ReadM a
maybeReader f = do
  arg  <- readerAsk
  maybe (readerError $ "cannot parse value `" ++ arg ++ "'") return . f $ arg

-- | Null 'Option' reader. All arguments will fail validation.
disabled :: ReadM a
disabled = readerError "disabled option"

-- modifiers --

-- | Specify a short name for an option.
short :: HasName f => Char -> Mod f a
short = fieldMod . name . OptShort

-- | Specify a long name for an option.
long :: HasName f => String -> Mod f a
long = fieldMod . name . OptLong

-- | Specify a default value for an option.
--
-- /Note/: Because this modifier means the parser will never fail,
-- do not use it with combinators such as 'some' or 'many', as
-- these combinators continue until a failure occurs.
-- Careless use will thus result in a hang.
--
-- To display the default value, combine with showDefault or
-- showDefaultWith.
value :: HasValue f => a -> Mod f a
value x = Mod id (DefaultProp (Just x) Nothing) id

-- | Specify a function to show the default value for an option.
showDefaultWith :: (a -> String) -> Mod f a
showDefaultWith s = Mod id (DefaultProp Nothing (Just s)) id

-- | Show the default value for this option using its 'Show' instance.
showDefault :: Show a => Mod f a
showDefault = showDefaultWith show

-- | Specify the help text for an option.
help :: String -> Mod f a
help s = optionMod $ \p -> p { propHelp = paragraph s }

-- | Specify the help text for an option as a 'Text.PrettyPrint.ANSI.Leijen.Doc'
-- value.
helpDoc :: Maybe Doc -> Mod f a
helpDoc doc = optionMod $ \p -> p { propHelp = Chunk doc }

-- | Specify the error to display when no argument is provided to this option.
noArgError :: ParseError -> Mod OptionFields a
noArgError e = fieldMod $ \p -> p { optNoArgError = const e }

-- | Specify a metavariable for the argument.
--
-- Metavariables have no effect on the actual parser, and only serve to specify
-- the symbolic name for an argument to be displayed in the help text.
metavar :: HasMetavar f => String -> Mod f a
metavar var = optionMod $ \p -> p { propMetaVar = var }

-- | Hide this option from the brief description.
--
-- Use 'internal' to hide the option from the help text too.
hidden :: Mod f a
hidden = optionMod $ \p ->
  p { propVisibility = min Hidden (propVisibility p) }

-- | Apply a function to the option description in the usage text.
--
-- > import Options.Applicative.Help
-- > flag' () (short 't' <> style bold)
--
-- /NOTE/: This builder is more flexible than its name and example
-- allude. One of the motivating examples for its addition was to
-- used `const` to completely replace the usage text of an option.
style :: ( Doc -> Doc ) -> Mod f a
style x = optionMod $ \p ->
  p { propDescMod = Just x }

-- | Add a command to a subparser option.
--
-- Suggested usage for multiple commands is to add them to a single subparser. e.g.
--
-- @
-- sample :: Parser Sample
-- sample = subparser
--        ( command "hello"
--          (info hello (progDesc "Print greeting"))
--       <> command "goodbye"
--          (info goodbye (progDesc "Say goodbye"))
--        )
-- @
command :: String -> ParserInfo a -> Mod CommandFields a
command cmd pinfo = fieldMod $ \p ->
  p { cmdCommands = (cmd, pinfo) : cmdCommands p }

-- | Add a description to a group of commands.
--
-- Advanced feature for separating logical groups of commands on the parse line.
--
-- If using the same `metavar` for each group of commands, it may yield a more
-- attractive usage text combined with `hidden` for some groups.
commandGroup :: String -> Mod CommandFields a
commandGroup g = fieldMod $ \p ->
  p { cmdGroup = Just g }

-- | Add a list of possible completion values.
completeWith :: HasCompleter f => [String] -> Mod f a
completeWith = completer . listCompleter

-- | Add a bash completion action. Common actions include @file@ and
-- @directory@. See
-- <http://www.gnu.org/software/bash/manual/html_node/Programmable-Completion-Builtins.html#Programmable-Completion-Builtins>
-- for a complete list.
action :: HasCompleter f => String -> Mod f a
action = completer . bashCompleter

-- | Add a completer to an argument.
--
-- A completer is a function String -> IO String which, given a partial
-- argument, returns all possible completions for that argument.
completer :: HasCompleter f => Completer -> Mod f a
completer f = fieldMod $ modCompleter (`mappend` f)

-- parsers --

-- | Builder for a command parser. The 'command' modifier can be used to
-- specify individual commands.
subparser :: Mod CommandFields a -> Parser a
subparser m = mkParser d g rdr
  where
    Mod _ d g = metavar "COMMAND" `mappend` m
    (groupName, cmds, subs) = mkCommand m
    rdr = CmdReader groupName cmds subs

-- | Builder for an argument parser.
argument :: ReadM a -> Mod ArgumentFields a -> Parser a
argument p m = mkParser d g (ArgReader rdr)
  where
    (Mod f d g) = noGlobal `mappend` m
    ArgumentFields compl = f (ArgumentFields mempty)
    rdr = CReader compl p

-- | Builder for a 'String' argument.
strArgument :: IsString s => Mod ArgumentFields s -> Parser s
strArgument = argument str

-- | Builder for a flag parser.
--
-- A flag that switches from a \"default value\" to an \"active value\" when
-- encountered. For a simple boolean value, use `switch` instead.
--
-- /Note/: Because this parser will never fail, it can not be used with
-- combinators such as 'some' or 'many', as these combinators continue until
-- a failure occurs. See @flag'@.
flag :: a                         -- ^ default value
     -> a                         -- ^ active value
     -> Mod FlagFields a          -- ^ option modifier
     -> Parser a
flag defv actv m = flag' actv m <|> pure defv

-- | Builder for a flag parser without a default value.
--
-- Same as 'flag', but with no default value. In particular, this flag will
-- never parse successfully by itself.
--
-- It still makes sense to use it as part of a composite parser. For example
--
-- > length <$> many (flag' () (short 't'))
--
-- is a parser that counts the number of "-t" arguments on the command line,
-- alternatively
--
-- > flag' True (long "on") <|> flag' False (long "off")
--
-- will require the user to enter '--on' or '--off' on the command line.
flag' :: a                         -- ^ active value
      -> Mod FlagFields a          -- ^ option modifier
      -> Parser a
flag' actv (Mod f d g) = mkParser d g rdr
  where
    rdr = let fields = f (FlagFields [] actv)
          in FlagReader (flagNames fields)
                        (flagActive fields)

-- | Builder for a boolean flag.
--
-- /Note/: Because this parser will never fail, it can not be used with
-- combinators such as 'some' or 'many', as these combinators continue until
-- a failure occurs. See @flag'@.
--
-- > switch = flag False True
switch :: Mod FlagFields Bool -> Parser Bool
switch = flag False True

-- | An option that always fails.
--
-- When this option is encountered, the option parser immediately aborts with
-- the given parse error.  If you simply want to output a message, use
-- 'infoOption' instead.
abortOption :: ParseError -> Mod OptionFields (a -> a) -> Parser (a -> a)
abortOption err m = option (readerAbort err) . (`mappend` m) $ mconcat
  [ noArgError err
  , value id
  , metavar "" ]

-- | An option that always fails and displays a message.
infoOption :: String -> Mod OptionFields (a -> a) -> Parser (a -> a)
infoOption = abortOption . InfoMsg

-- | Builder for an option taking a 'String' argument.
strOption :: IsString s => Mod OptionFields s -> Parser s
strOption = option str

-- | Builder for an option using the given reader.
--
-- This is a regular option, and should always have either a @long@ or
-- @short@ name specified in the modifiers (or both).
--
-- > nameParser = option str ( long "name" <> short 'n' )
--
option :: ReadM a -> Mod OptionFields a -> Parser a
option r m = mkParser d g rdr
  where
    Mod f d g = metavar "ARG" `mappend` m
    fields = f (OptionFields [] mempty ExpectsArgError)
    crdr = CReader (optCompleter fields) r
    rdr = OptReader (optNames fields) crdr (optNoArgError fields)

-- | Modifier for 'ParserInfo'.
newtype InfoMod a = InfoMod
  { applyInfoMod :: ParserInfo a -> ParserInfo a }

instance Monoid (InfoMod a) where
  mempty = InfoMod id
  mappend = (<>)

instance Semigroup (InfoMod a) where
  m1 <> m2 = InfoMod $ applyInfoMod m2 . applyInfoMod m1

-- | Show a full description in the help text of this parser.
fullDesc :: InfoMod a
fullDesc = InfoMod $ \i -> i { infoFullDesc = True }

-- | Only show a brief description in the help text of this parser.
briefDesc :: InfoMod a
briefDesc = InfoMod $ \i -> i { infoFullDesc = False }

-- | Specify a header for this parser.
header :: String -> InfoMod a
header s = InfoMod $ \i -> i { infoHeader = paragraph s }

-- | Specify a header for this parser as a 'Text.PrettyPrint.ANSI.Leijen.Doc'
-- value.
headerDoc :: Maybe Doc -> InfoMod a
headerDoc doc = InfoMod $ \i -> i { infoHeader = Chunk doc }

-- | Specify a footer for this parser.
footer :: String -> InfoMod a
footer s = InfoMod $ \i -> i { infoFooter = paragraph s }

-- | Specify a footer for this parser as a 'Text.PrettyPrint.ANSI.Leijen.Doc'
-- value.
footerDoc :: Maybe Doc -> InfoMod a
footerDoc doc = InfoMod $ \i -> i { infoFooter = Chunk doc }

-- | Specify a short program description.
progDesc :: String -> InfoMod a
progDesc s = InfoMod $ \i -> i { infoProgDesc = paragraph s }

-- | Specify a short program description as a 'Text.PrettyPrint.ANSI.Leijen.Doc'
-- value.
progDescDoc :: Maybe Doc -> InfoMod a
progDescDoc doc = InfoMod $ \i -> i { infoProgDesc = Chunk doc }

-- | Specify an exit code if a parse error occurs.
failureCode :: Int -> InfoMod a
failureCode n = InfoMod $ \i -> i { infoFailureCode = n }

-- | Disable parsing of regular options after arguments. After a positional
--   argument is parsed, all remaining options and arguments will be treated
--   as a positional arguments. Not recommended in general as users often
--   expect to be able to freely intersperse regular options and flags within
--   command line options.
noIntersperse :: InfoMod a
noIntersperse = InfoMod $ \p -> p { infoPolicy = NoIntersperse }

-- | Intersperse matched options and arguments normally, but allow unmatched
--   options to be treated as positional arguments.
--   This is sometimes useful if one is wrapping a third party cli tool and
--   needs to pass options through, while also providing a handful of their
--   own options. Not recommended in general as typos by the user may not
--   yield a parse error and cause confusion.
forwardOptions :: InfoMod a
forwardOptions = InfoMod $ \p -> p { infoPolicy = ForwardOptions }

-- | Disable parsing of regular options completely. All options and arguments
--   will be treated as a positional arguments. Obviously not recommended in
--   general as options will be unreachable.
--   This is the same behaviour one sees after the "--" pseudo-argument.
allPositional :: InfoMod a
allPositional = InfoMod $ \p -> p { infoPolicy = AllPositionals }


-- | Create a 'ParserInfo' given a 'Parser' and a modifier.
info :: Parser a -> InfoMod a -> ParserInfo a
info parser m = applyInfoMod m base
  where
    base = ParserInfo
      { infoParser = parser
      , infoFullDesc = True
      , infoProgDesc = mempty
      , infoHeader = mempty
      , infoFooter = mempty
      , infoFailureCode = 1
      , infoPolicy = Intersperse }

newtype PrefsMod = PrefsMod
  { applyPrefsMod :: ParserPrefs -> ParserPrefs }

instance Monoid PrefsMod where
  mempty = PrefsMod id
  mappend = (<>)

instance Semigroup PrefsMod where
  m1 <> m2 = PrefsMod $ applyPrefsMod m2 . applyPrefsMod m1

-- | Include a suffix to attach to the metavar when multiple values
--   can be entered.
multiSuffix :: String -> PrefsMod
multiSuffix s = PrefsMod $ \p -> p { prefMultiSuffix = s }

-- | Turn on disambiguation.
--
--   See
--   https://github.com/pcapriotti/optparse-applicative#disambiguation
disambiguate :: PrefsMod
disambiguate = PrefsMod $ \p -> p { prefDisambiguate = True }

-- | Show full help text on any error.
showHelpOnError :: PrefsMod
showHelpOnError = PrefsMod $ \p -> p { prefShowHelpOnError = True }

-- | Show the help text if the user enters only the program name or
--   subcommand.
--
--   This will suppress a "Missing:" error and show the full usage
--   instead if a user just types the name of the program.
showHelpOnEmpty :: PrefsMod
showHelpOnEmpty = PrefsMod $ \p -> p { prefShowHelpOnEmpty = True }

-- | Turn off backtracking after subcommand is parsed.
noBacktrack :: PrefsMod
noBacktrack = PrefsMod $ \p -> p { prefBacktrack = NoBacktrack }

-- | Allow full mixing of subcommand and parent arguments by inlining
-- selected subparsers into the parent parser.
--
-- /NOTE:/ When this option is used, preferences for the subparser which
-- effect the parser behaviour (such as noIntersperse) are ignored.
subparserInline :: PrefsMod
subparserInline = PrefsMod $ \p -> p { prefBacktrack = SubparserInline }

-- | Set the maximum width of the generated help text.
columns :: Int -> PrefsMod
columns cols = PrefsMod $ \p -> p { prefColumns = cols }

-- | Show equals sign, rather than space, in usage and help text for options with
-- long names.
helpLongEquals :: PrefsMod
helpLongEquals = PrefsMod $ \p -> p { prefHelpLongEquals = True }

-- | Show global help information in subparser usage
helpShowGlobals :: PrefsMod
helpShowGlobals = PrefsMod $ \p -> p { prefHelpShowGlobal = True}


-- | Create a `ParserPrefs` given a modifier
prefs :: PrefsMod -> ParserPrefs
prefs m = applyPrefsMod m base
  where
    base = ParserPrefs
      { prefMultiSuffix = ""
      , prefDisambiguate = False
      , prefShowHelpOnError = False
      , prefShowHelpOnEmpty = False
      , prefBacktrack = Backtrack
      , prefColumns = 80
      , prefHelpLongEquals = False
      , prefHelpShowGlobal = False }

-- Convenience shortcuts

-- | Trivial option modifier.
idm :: Monoid m => m
idm = mempty

-- | Default preferences.
defaultPrefs :: ParserPrefs
defaultPrefs = prefs idm
