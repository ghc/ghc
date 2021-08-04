module Options.Applicative (
  -- * Applicative option parsers
  --
  -- | This module exports all one should need for defining and using
  -- optparse-applicative command line option parsers.
  --
  -- See <https://github.com/pcapriotti/optparse-applicative> for a tutorial,
  -- and a general introduction to applicative option parsers.
  --
  -- See the sections below for more detail

  -- * Exported modules
  --
  -- | The standard @Applicative@ module is re-exported here for convenience.
  module Control.Applicative,

  -- * Option Parsers
  --
  -- | A 'Parser' is the core type in optparse-applicative. A value of type
  -- @Parser a@ represents a specification for a set of options, which will
  -- yield a value of type a when the command line arguments are successfully
  -- parsed.
  --
  -- There are several types of primitive 'Parser'.
  --
  -- * Flags: simple no-argument options. When a flag is encountered on the
  -- command line, its value is returned.
  --
  -- * Options: options with an argument. An option can define a /reader/,
  -- which converts its argument from String to the desired value, or throws a
  -- parse error if the argument does not validate correctly.
  --
  -- * Arguments: positional arguments, validated in the same way as option
  -- arguments.
  --
  -- * Commands. A command defines a completely independent sub-parser. When a
  -- command is encountered, the whole command line is passed to the
  -- corresponding parser.
  --
  -- See the "Parser Builders" section for how to construct and customise
  -- these parsers.
  Parser,

  -- ** Parser builders
  --
  -- | This section contains utility functions and combinators to create parsers
  -- for individual options.
  --
  -- Each parser builder takes an option modifier. A modifier can be created by
  -- composing the basic modifiers provided by here using the 'Monoid' operations
  -- 'mempty' and 'mappend', or their aliases 'idm' and '<>'.
  --
  -- For example:
  --
  -- > out = strOption
  -- >     ( long "output"
  -- >    <> short 'o'
  -- >    <> metavar "FILENAME" )
  --
  -- creates a parser for an option called \"output\".
  flag,
  flag',
  switch,

  strOption,
  option,

  strArgument,
  argument,

  subparser,
  hsubparser,

  abortOption,
  infoOption,
  helper,

  -- ** Modifiers
  --
  -- | 'Parser' builders take a modifier, which represents a modification of the
  -- properties of an option, and can be composed as a monoid.
  --
  -- Contraints are often used to ensure that the modifiers can be sensibly applied.
  -- For example, positional arguments can't be specified by long or short names,
  -- so the 'HasName' constraint is used to ensure we have a flag or option.
  Mod,

  short,
  long,
  help,
  helpDoc,
  value,
  showDefaultWith,
  showDefault,
  metavar,
  noArgError,
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

  OptionFields,
  FlagFields,
  ArgumentFields,
  CommandFields,

  HasName,
  HasCompleter,
  HasValue,
  HasMetavar,
  -- ** Readers
  --
  -- | A reader is used by the 'option' and 'argument' builders to parse
  -- the data passed by the user on the command line into a data type.
  --
  -- The most common are 'str' which is used for 'String' like types,
  -- including 'ByteString' and 'Text'; and 'auto', which uses the 'Read'
  -- typeclass, and is good for simple types like 'Int' or 'Double'.
  --
  -- More complex types can use the 'eitherReader' or 'maybeReader'
  -- functions to pattern match or use a more expressive parser like a
  -- member of the 'Parsec' family.
  ReadM,

  auto,
  str,
  maybeReader,
  eitherReader,
  disabled,
  readerAbort,
  readerError,

  -- * Program descriptions
  --
  -- ** 'ParserInfo'
  --
  -- | A 'ParserInfo' describes a command line program, used to generate a help
  -- screen. Two help modes are supported: brief and full. In brief mode, only
  -- an option and argument summary is displayed, while in full mode each
  -- available option and command, including hidden ones, is described.
  --
  -- A 'ParserInfo' should be created with the 'info' function and a set of
  -- 'InfoMod' modifiers.
  --
  info,

  ParserInfo(..),

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

  -- * Running parsers
  --
  -- | The execParser family of functions are used to run parsers
  execParser,
  customExecParser,
  execParserPure,

  -- ** Handling parser results manually
  getParseResult,
  handleParseResult,
  parserFailure,
  renderFailure,
  overFailure,

  -- ** 'ParserPrefs'
  --
  -- | A 'ParserPrefs' contains general preferences for all command-line
  -- options, and should be built with the 'prefs' function.
  prefs,

  ParserPrefs(..),

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
  defaultPrefs,

  -- * Completions
  --
  -- | optparse-applicative supplies a rich completion system for bash,
  -- zsh, and fish shells.
  --
  -- 'Completer' functions are used for option and argument to complete
  -- their values.
  --
  -- Use the 'completer' builder to use these.
  -- The 'action' and 'completeWith' builders are also provided for
  -- convenience, to use 'bashCompleter' and 'listCompleter' as a 'Mod'.
  Completer,
  mkCompleter,
  listIOCompleter,

  listCompleter,
  bashCompleter,

  -- * Types
  ParseError(..),
  ParserHelp(..),
  ParserFailure(..),
  ParserResult(..),
  CompletionResult(..)

  ) where

-- reexport Applicative here for convenience
import Control.Applicative

import Options.Applicative.Common
import Options.Applicative.Builder
import Options.Applicative.Builder.Completer
import Options.Applicative.Extra
import Options.Applicative.Types
