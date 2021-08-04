# optparse-applicative

[![Continuous Integration status][status-png]][status]
[![Hackage matrix][hackage-matrix-png]][hackage-matrix]
[![Hackage page (downloads and API reference)][hackage-png]][hackage]
[![Hackage-Deps][hackage-deps-png]][hackage-deps]


optparse-applicative is a haskell library for parsing options on
the command line, and providing a powerful [applicative] interface
for composing them.

optparse-applicative takes care of reading and validating the
arguments passed to the command line, handling and reporting errors,
generating a usage line, a comprehensive help screen, and enabling
context-sensitive bash, zsh, and fish completions.

**Table of Contents**

- [Introduction](#introduction)
- [Quick Start](#quick-start)
- [Basics](#basics)
    - [Parsers](#parsers)
    - [Applicative](#applicative)
    - [Alternative](#alternative)
    - [Running parsers](#running-parsers)
- [Builders](#builders)
    - [Regular options](#regular-options)
    - [Flags](#flags)
    - [Arguments](#arguments)
    - [Commands](#commands)
    - [Modifiers](#modifiers)
- [Custom parsing and error handling](#custom-parsing-and-error-handling)
    - [Parser runners](#parser-runners)
    - [Option readers](#option-readers)
    - [Preferences](#preferences)
    - [Disambiguation](#disambiguation)
    - [Customising the help screen](#customising-the-help-screen)
    - [Command Groups](#command-groups)
- [Bash completion](#bash-zsh-and-fish-completions)
    - [Actions and completers](#actions-and-completers)
    - [Internals](#internals)
- [Arrow interface](#arrow-interface)
- [Applicative Do](#applicative-do)
- [FAQ](#faq)
- [How it works](#how-it-works)

## Introduction

The core type in optparse-applicative is a `Parser`

```haskell
data Parser a

instance Functor Parser
instance Applicative Parser
instance Alternative Parser
```

A value of type `Parser a` represents a specification for a set of
options, which will yield a value of type `a` when the command line
arguments are successfully parsed.

If you are familiar with parser combinator libraries like [parsec],
[attoparsec], or the json parser [aeson] you will feel right at
home with optparse-applicative.

If not, don't worry! All you really need to learn are a few basic
parsers, and how to compose them as instances of `Applicative` and
`Alternative`.

## Quick Start

Here's a simple example of a parser.

```haskell
import Options.Applicative
import Data.Semigroup ((<>))

data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }

sample :: Parser Sample
sample = Sample
      <$> strOption
          ( long "hello"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"
         <> help "How enthusiastically to greet"
         <> showDefault
         <> value 1
         <> metavar "INT" )
```

The parser is built using an [applicative] style starting from a
set of basic combinators. In this example, hello is defined as an
option with a `String` argument, while quiet is a boolean flag
(called a switch) and enthusiasm gets parsed as an `Int` with help
of the `Read` type class.


The parser can be used like this:

```haskell
main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

greet :: Sample -> IO ()
greet (Sample h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
greet _ = return ()
```

The `greet` function is the entry point of the program, while `opts`
is a complete description of the program, used when generating a
help text. The `helper` combinator takes any parser, and adds a
`help` option to it.

The `hello` option in this example is mandatory since it doesn't
have a default value, so running the program without any argument
will display an appropriate error message and a short option summary:

    Missing: --hello TARGET

    Usage: hello --hello TARGET [-q|--quiet] [--enthusiasm INT]
      Print a greeting for TARGET

Running the program with the `--help` option will display the full help text
containing a detailed list of options with descriptions

```
    hello - a test for optparse-applicative

    Usage: hello --hello TARGET [-q|--quiet] [--enthusiasm INT]
      Print a greeting for TARGET

    Available options:
      --hello TARGET           Target for the greeting
      -q,--quiet               Whether to be quiet
      --enthusiasm INT         How enthusiastically to greet (default: 1)
      -h,--help                Show this help text
```

## Basics
### Parsers

optparse-applicative provides a number of primitive parsers,
corresponding to different posix style options, through its *Builder*
interface. These are detailed in their [own section](#builders)
below, for now, here's a look at a few more examples to get a feel
for how parsers can be defined.


Here is a parser for a mandatory option with an argument:

```haskell
target :: Parser String
target = strOption
  (  long "hello"
  <> metavar "TARGET"
  <> help "Target for the greeting" )
```

One can see that we are defining an option parser for a `String`
argument, with *long* option name "hello", *metavariable* "TARGET",
and the given help text. This means that the `target` parser defined
above will require an option like

    --hello world

on the command line. The metavariable and the help text will appear
in the generated help text, but don't otherwise affect the behaviour
of the parser.

The attributes passed to the option are called *modifiers*, and are
composed using the [semigroup] operation `(<>)`.

Options with an argument such as `target` are referred to as *regular
options*, and are very common.  Another type of option is a *flag*,
the simplest of which is a boolean *switch*, for example:

```haskell
quiet :: Parser Bool
quiet = switch ( long "quiet" <> short 'q' <> help "Whether to be quiet" )
```

Here we used a `short` modifier to specify a one-letter name for
the option.  This means that this switch can be set either with
`--quiet` or `-q`.

Flags, unlike regular options, have no arguments. They simply return
a predetermined value. For the simple switch above, this is `True`
if the user types the flag, and `False` otherwise.

There are other kinds of basic parsers, and several ways to configure
them.  These are covered in the [Builders](#builders) section.

### Applicative

Now we may combine the `target` and `quiet` into a single parser that
accepts both options and returns a combined value. Given a type

```haskell
data Options = Options
  { optTarget :: String
  , optQuiet :: Bool }
```

and now it's just a matter of using `Applicative`'s apply operator `(<*>)`
to combine the two previously defined parsers

```haskell
opts :: Parser Options
opts = Options <$> target <*> quiet
```

No matter which parsers appear first in the sequence, options will
still be parsed in whatever order they appear in the command line.
A parser with such a property is sometimes called a *permutation
parser*.

In our example, a command line like:

    --target world -q

will give the same result as

    -q --target world

It is this property which leads us to an Applicative interface
instead of a Monadic one, as all options must be considered in
parallel, and can not depend on the output of other options.

Note, however, that the order of sequencing is still somewhat
significant, in that it affects the generated help text. Customisation
can be achieved easily through a lambda abstraction, with [Arrow
notation](#arrow-interface), or by taking advantage of GHC 8's
[ApplicativeDo](#applicative-do) extension.

### Alternative

It is also common to find programs that can be configured in different
ways through the command line.  A typical example is a program that
can be given a text file as input, or alternatively read it directly
from the standard input.

We can model this easily and effectively in Haskell using *sum types*:

```haskell
data Input
  = FileInput FilePath
  | StdInput

run :: Input -> IO ()
run = ...
```

We can now define two basic parsers for the components of the sum type:

```haskell
fileInput :: Parser Input
fileInput = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Input file" )

stdInput :: Parser Input
stdInput = flag' StdInput
  (  long "stdin"
  <> help "Read from stdin" )
```

As the `Parser` type constructor is an instance of `Alternative`, we can
compose these parsers with a choice operator `(<|>)`

```haskell
input :: Parser Input
input = fileInput <|> stdInput
```

Now `--file "foo.txt"` will be parsed as `FileInput "foo.txt"`, `--stdin`
will be parsed as `StdInput`, but a command line containing both options,
like

    --file "foo.txt" --stdin

will be rejected.

Having `Applicative` and `Alternative` instances, optparse-applicative
parsers are also able to be composed with standard combinators. For
example: `optional :: Alternative f => f a -> f (Maybe a)` will
mean the user is not required to provide input for the affected
`Parser`.

### Running parsers

Before we can run a `Parser`, we need to wrap it into a `ParserInfo`
structure, that specifies a number of properties that only apply
to top level parsers, such as a header describing what the program
does, to be displayed in the help screen.

The function `info` will help with this step.  In the [Quick Start](#quick-start)
we saw

```haskell
opts :: ParserInfo Sample
opts = info (sample <**> helper)
  ( fullDesc
  <> progDesc "Print a greeting for TARGET"
  <> header "hello - a test for optparse-applicative" )
```

The `helper` parser that we added after `opts` just creates a dummy
`--help` option that displays the help text.  Besides that, we just
set some of the fields of the `ParserInfo` structure with meaningful
values.  Now that we have a `ParserInfo`, we can finally run the
parser.  The simplest way to do so is to simply call the `execParser`
function in your `main`:

```haskell
main :: IO ()
main = do
  options <- execParser opts
  ...
```

The `execParser` function takes care of everything, including getting
the arguments from the command line, displaying errors and help
screens to the user, and exiting with an appropriate exit code.

There are other ways to run a `ParserInfo`, in situations where you
need finer control over the behaviour of your parser, or if you
want to use it in pure code. They will be covered in [Custom parsing
and error handling](#custom-parsing-and-error-handling).

## Builders

Builders allow you to define parsers using a convenient combinator-based
syntax. We have already seen examples of builders in action, like
`strOption` and `switch`, which we used to define the `opts` parser
for our "hello" example.

Builders always take a [modifier](#modifiers) argument, which is
essentially a composition of functions acting on the option, setting
values for properties or adding features.

Builders work by building the option from scratch, and eventually
lifting it to a single-option parser, ready to be combined with
other parsers using normal `Applicative` and `Alternative` combinators.

See the [haddock documentation][hackage] for `Options.Applicative.Builder`
for a full list of builders and modifiers.

There are four different kinds of options in `optparse-applicative`:
regular options, flags, arguments, and commands. In the following,
we will go over each one of these and describe the builders that
can be used to create them.

### Regular options

A *regular option* is an option which takes a single argument,
parses it, and returns a value.

A regular option can have a default value, which is used as the
result if the option is not found in the command line. An option
without a default value is considered mandatory, and produces an
error when not found.

Regular options can have *long* names, or *short* (one-character)
names, which determine when the option matches and how the argument
is extracted.

An option with a long name (say "output") is specified on the command
line as


    --output filename.txt

or

    --output=filename.txt

while a short name option (say "o") can be specified with

    -o filename.txt

or

    -ofilename.txt

Options can have more than one name, usually one long and one short,
although you are free to create options with an arbitrary combination
of long and short names.

Regular options returning strings are the most common, and they can
be created using the `strOption` builder. For example,

```haskell
strOption
   ( long "output"
  <> short 'o'
  <> metavar "FILE"
  <> value "out.txt"
  <> help "Write output to FILE" )
```

creates a regular option with a string argument (which can be
referred to as `FILE` in the help text and documentation), default
value "out.txt", a long name "output" and a short name "o".

A regular `option` can return an object of any type, and takes a
*reader* parameter which specifies how the argument should be parsed.
A common reader is `auto`, which requires a `Read` instance for the
return type and uses it to parse its argument. For example:

```haskell
lineCount :: Parser Int
lineCount = option auto
            ( long "lines"
           <> short 'n'
           <> metavar "K"
           <> help "Output the last K lines" )
```

specifies a regular option with an `Int` argument. We added an
explicit type annotation here, since without it the parser would
have been polymorphic in the output type. There's usually no need
to add type annotations, however, because the type will be normally
inferred from the context in which the parser is used.

Further information on *readers* is available [below](#option-readers).

### Flags

A *flag* is just like a regular option, but it doesn't take any
arguments, it is either present in the command line or not.

A flag has a default value and an *active value*. If the flag is
found on the command line, the active value is returned, otherwise
the default value is used. For example:

```haskell
data Verbosity = Normal | Verbose

flag Normal Verbose
  ( long "verbose"
 <> short 'v'
 <> help "Enable verbose mode" )
```

is a flag parser returning a `Verbosity` value.

Simple boolean flags can be specified using the `switch` builder, like so:

```haskell
switch
  ( long "keep-tmp-files"
 <> help "Retain all intermediate temporary files" )
```

There is also a `flag'` builder, which has no default value. This
was demonstrated earlier for our `--stdin` flag example, and is
usually used as one side of an alternative.

Another interesting use for the `flag'` builder is to count the
number of instances on the command line, for example, verbosity
settings could be specified on a scale; the following parser will
count the number of instances of `-v` on the command line.

```haskell
length <$> many (flag' () (short 'v'))
```

Flags can be used together after a single hyphen, so  `-vvv` and
`-v -v -v` will both yield 3 for the above parser.

### Arguments

An *argument* parser specifies a positional command line argument.

The `argument` builder takes a reader parameter, and creates a
parser which will return the parsed value every time it is passed
a command line argument for which the reader succeeds. For example

```haskell
argument str (metavar "FILE")
```

creates an argument accepting any string.  To accept an arbitrary
number of arguments, combine the `argument` builder with either the
`many` or `some` combinator:

```haskell
some (argument str (metavar "FILES..."))
```

Note that arguments starting with `-` are considered options by
default, and will not be considered by an `argument` parser.

However, parsers always accept a special argument: `--`. When a
`--` is found on the command line, all the following words are
considered by `argument` parsers, regardless of whether they start
with `-` or not.

Arguments use the same *readers* as regular options.

### Commands

A *command* can be used to specify a sub-parser to be used when a
certain string is encountered in the command line.

Commands are useful to implement command line programs with multiple
functions, each with its own set of options, and possibly some
global options that apply to all of them. Typical examples are
version control systems like `git`, or build tools like `cabal`.

A command can be created using the `subparser` builder (or `hsubparser`,
which is identical but for an additional `--help` option on each
command), and commands can be added with the `command` modifier.
For example

```haskell
subparser
  ( command "add" (info addOptions ( progDesc "Add a file to the repository" ))
 <> command "commit" (info commitOptions ( progDesc "Record changes to the repository" ))
  )
```

Each command takes a full `ParserInfo` structure, which will be
used to extract a description for this command when generating a
help text.

Note that all the parsers appearing in a command need to have the
same type.  For this reason, it is often best to use a sum type
which has the same structure as the command itself. For example,
for the parser above, you would define a type like:

```haskell
data Options = Options
  { optCommand :: Command
  , ... }

data Command
  = Add AddOptions
  | Commit CommitOptions
  ...
```

Alternatively, you can directly return an `IO` action from a parser,
and execute it using `join` from `Control.Monad`.

```haskell
start :: String -> IO ()
stop :: IO ()

opts :: Parser (IO ())
opts = subparser
  ( command "start" (info (start <$> argument str idm) idm)
 <> command "stop"  (info (pure stop) idm) )

main :: IO ()
main = join $ execParser (info opts idm)
```

### Modifiers

*Modifiers* are instances of the `Semigroup` and `Monoid` typeclasses,
so they can be combined using the composition function `mappend`
(or simply `(<>)`).  Since different builders accept different sets
of modifiers, modifiers have a type parameter that specifies which
builders support it.

For example,

```haskell
command :: String -> ParserInfo a -> Mod CommandFields a
```

can only be used with [commands](#commands), as the `CommandFields`
type argument of `Mod` will prevent it from being passed to builders
for other types of options.

Many modifiers are polymorphic in this type argument, which means
that they can be used with any builder.

## Custom parsing and error handling

### Parser runners
Parsers are run with the `execParser` family of functions — from
easiest to use to most flexible these are:

```haskell
execParser       :: ParserInfo a -> IO a
customExecParser :: ParserPrefs -> ParserInfo a -> IO a
execParserPure   :: ParserPrefs -> ParserInfo a -> [String] -> ParserResult a
```

When using the `IO` functions, retrieving command line arguments
and handling exit codes and failure will be done automatically.
When using `execParserPure`, the functions

```haskell
handleParseResult :: ParserResult a -> IO a
overFailure :: (ParserHelp -> ParserHelp) -> ParserResult a -> ParserResult a
```

can be used to correctly set exit codes and display the help message;
and modify the help message in the event of a failure (adding
additional information for example).

### Option readers

Options and Arguments require a way to interpret the string passed
on the command line to the type desired. The `str` and `auto`
*readers* are the most common way, but one can also create a custom
reader that doesn't use the `Read` type class or return a `String`,
and use it to parse the option. A custom reader is a value in the
`ReadM` monad.

We provide the `eitherReader :: (String -> Either String a) -> ReadM a`
convenience function to help create these values, where a `Left` will
hold the error message for a parse failure.

```haskell
data FluxCapacitor = ...

parseFluxCapacitor :: ReadM FluxCapacitor
parseFluxCapacitor = eitherReader $ \s -> ...

option parseFluxCapacitor ( long "flux-capacitor" )
```

One can also use `ReadM` directly, using `readerAsk` to obtain the
command line string, and `readerAbort` or `readerError` within the
`ReadM` monad to exit with an error message.

One nice property of `eitherReader` is how well it composes with
[attoparsec] parsers with

```haskell
import qualified Data.Attoparsec.Text as A
attoReadM :: A.Parser a -> ReadM a
attoReadM p = eitherReader (A.parseOnly p . T.pack)
```

### Preferences
`PrefsMod`s can be used to customise the look of the usage text and
control when it is displayed; turn off backtracking of subparsers;
and turn on [disambiguation](#disambiguation).

To use these modifications, provide them to the `prefs` builder,
and pass the resulting preferences to one of the parser runners
that take an `ParserPrefs` parameter, like `customExecParser`.


### Disambiguation

It is possible to configure optparse-applicative to perform automatic
disambiguation of prefixes of long options. For example, given a
program `foo` with options `--filename` and `--filler`, typing

    $ foo --fil test.txt

fails, whereas typing

    $ foo --file test.txt

succeeds, and correctly identifies `"file"` as an unambiguous prefix
of the `filename` option.

Option disambiguation is *off* by default. To enable it, use the
`disambiguate` `PrefsMod` modifier as described above.

Here is a minimal example:

```haskell
import Options.Applicative

sample :: Parser ()
sample = () <$
  switch (long "filename") <*
  switch (long "filler")

main :: IO ()
main = customExecParser p opts
  where
    opts = info (helper <*> sample) idm
    p = prefs disambiguate

```

### Customising the help screen

optparse-applicative has a number of combinators to help customise
the usage text, and determine when it should be displayed.

The `progDesc`, `header`, and `footer` functions can be used to
specify a brief description or tagline for the program, and detailed
information surrounding the generated option and command descriptions.

Internally we actually use the [ansi-wl-pprint][ansi-wl-pprint]
library, and one can use the `headerDoc` combinator and friends if
additional customisation is required.

To display the usage text, the user may type `--help` if the `helper`
combinator has been applied to the `Parser`.

Authors can also use the preferences `showHelpOnError` or
`showHelpOnEmpty` to show the help text on any parser failure or
when a command is not complete and at the beginning of the parse
of the main program or one of its subcommands respectively.

Even if the help text is not shown for an error, a specific error
message will be, indicating what's missing, or what was unable to
be parsed.

```haskell
myParser :: Parser ()
myParser = ...

main :: IO ()
main = customExecParser p opts
  where
    opts = info (myParser <**> helper) idm
    p = prefs showHelpOnEmpty
```

### Command groups

One experimental feature which may be useful for programs with many
subcommands is command group separation.

```haskell
data Sample
  = Hello [String]
  | Goodbye
  deriving (Eq, Show)

hello :: Parser Sample
hello = Hello <$> many (argument str (metavar "TARGET..."))

sample :: Parser Sample
sample = subparser
       ( command "hello" (info hello (progDesc "Print greeting"))
      <> command "goodbye" (info (pure Goodbye) (progDesc "Say goodbye"))
       )
      <|> subparser
       ( command "bonjour" (info hello (progDesc "Print greeting"))
      <> command "au-revoir" (info (pure Goodbye) (progDesc "Say goodbye"))
      <> commandGroup "French commands:"
      <> hidden
       )
```

This will logically separate the usage text for the two subparsers
(these would normally appear together if the `commandGroup` modifier
was not used). The `hidden` modifier suppresses the metavariable
for the second subparser being show in the brief usage line, which
is desirable in some cases.

In this example we have essentially created synonyms for our parser,
but one could use this to separate common commands from rare ones,
or safe from dangerous.

The usage text for the preceding example is:
```
Usage: commands COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  hello                    Print greeting
  goodbye                  Say goodbye

French commands:
  bonjour                  Print greeting
  au-revoir                Say goodbye
```

## Bash, Zsh, and Fish Completions

`optparse-applicative` has built-in support for the completion of
command line options and arguments in bash, zsh, and fish shells.
Any parser, when run using the `execParser` family of functions,
is automatically extended with a few (hidden) options for the
completion system:

 - `--bash-completion-script`: this takes the full path of the program as
   argument, and prints a bash script, which, when sourced into a bash session,
   will install the necessary machinery to make bash completion work. For a
   quick test, you can run something like (for a program called `foo` on the
   `PATH`):

   ```console
   $ source <(foo --bash-completion-script `which foo`)
   ```

   Normally, the output of `--bash-completion-script` should be shipped with
   the program and copied to the appropriate directory (usually
   `/etc/bash_completion.d/`) during installation;

 - `--zsh-completion-script`: which is analogous for zsh;

 - `--fish-completion-script`: which is analogous for fish shell;

 - `--bash-completion-index`, `--bash-completion-word`: internal options used
   by the completion script to obtain a list of possible completions for a
   given command line;

 - `--bash-completion-enriched`: a flag to tell the completion system to emit
   descriptions along with possible completions. This is used to provide help
   along with the completion for `zsh` and `fish`.

### Actions and completers

By default, options and commands are always completed. So, for example, if the
program `foo` has an option with a long name `output`, typing

```console
$ foo --ou<TAB>
```

will complete `--output` automatically.

Arguments (either of regular options, or top-level) are not completed by
default. To enable completion for arguments, use one of the following modifiers
on a regular option or argument:

 - `completeWith`: specifies a list of possible completions to choose from;
 - `action`: specifies a completion "action". An action dynamically determines
   a list of possible completions. Common actions are "file" and "directory";
   the full list of actions can be found in the [bash documentation];
 - `completer`: a completer is a function `String -> IO [String]`, returning
   all possible completions for a given string. You can use this modifier to
   specify a custom completion for an argument.

Completion modifiers can be used multiple times: the resulting completions will
call all of them and join the results.

### Internals

When running a parser with `execParser`, the parser is extended with
`bashCompletionParser`, which defines the above options.

When completion is triggered, the completion script calls the executable with
the special `--bash-completion-index` and `--bash-completion-word` options.

The original parser is therefore run in *completion mode*, i.e. `runParser` is
called on a different monad, which keeps track of the current state of the
parser, and exits when all arguments have been processed.

The completion monad returns, on failure, either the last state of the parser
(if no option could be matched), or the completer associated to an option (if
it failed while fetching the argument for that option).

From that we generate a list of possible completions, and print them to
standard output. They are then read by the completion script and put into the
`COMPREPLY` variable (or an appropriate alternative for the other shells).

## Arrow interface

It is also possible to use the [Arrow syntax][arrows] to combine basic parsers.

This can be particularly useful when the structure holding parse results is
deeply nested, or when the order of fields differs from the order in which the
parsers should be applied.

Using functions from the `Options.Applicative.Arrows` module, one can write,
for example:

```haskell
data Options = Options
  { optArgs :: [String]
  , optVerbose :: Bool }

opts :: Parser Options
opts = runA $ proc () -> do
  verbosity  <- asA (option auto (short 'v' <> value 0)) -< ()
  let verbose = verbosity > 0
  args       <- asA (many (argument str idm)) -< ()
  returnA -< Options args verbose
```

where parsers are converted to arrows using `asA`, and the resulting
composed arrow is converted back to a `Parser` with `runA`.

See `tests/Examples/Cabal.hs` for a slightly more elaborate example
using the arrow syntax for defining parsers.

Note that the `Arrow` interface is provided only for convenience. The
API based on `Applicative` is just as expressive, although it might be
cumbersome to use in certain cases.

## Applicative do

Some may find using optparse-applicative easier using do notation.
However, as `Parser` is not an instance of `Monad`, this can only
be done in recent versions of GHC using the *ApplicativeDo* extension.
For example, a parser specified in this manner might be

```haskell
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ApplicativeDo              #-}

data Options = Options
  { optArgs :: [String]
  , optVerbose :: Bool }

opts :: Parser Options
opts = do
  optVerbose    <- switch (short 'v')
  optArgs       <- many (argument str idm)
  pure Options {..}
```

Here we've also used the *RecordWildCards* extension to make the
parser specification cleaner. Compilation errors referring to `Monad`
instances not being found are likely because the `Parser` specified
can not be implemented entirely with `Applicative` (Note however,
there were a few desugaring bugs regarding ApplicativeDo in GHC
8.0.1, function application with `($)` in particular may not work,
and the `pure` value should instead be wrapped parenthetically).

## FAQ

* Monadic parsing?

  If a Monadic style were to be used, there would be no possible
  way to traverse the parser and generate a usage string, or for
  us to allow for options to be parsed in any order. Therefore it
  is intentionally unsupported to write a `Parser` in this manner
  with optparse-applicative, and the `Parser` type does not have
  an instance for `Monad`.

* Overlapping flags and options / options with optional arguments?

  This is not supported as it can lead to an ambiguous parse.

  For example, if we supported and had an optional value option
  "--foo" and a flag "--bar", is "--foo --bar" the option with value
  "--bar", or the default value with the flag switched on? What if
  instead of a switch we had many positional string arguments, is
  the first string the option's value or the first positional?

  It is suggested to instead use the `Alternative` instance of
  `Parser` and create a flag', an option, and a pure value for the
  default (with different names for the flag and option).

* Backtracking on `ReadM` errors?

  Parser structures are predetermined at parse time. This means
  that if a `ReadM` fails, the whole parse must also fail, we can't
  consider any alternatives, as there can be no guarantee that the
  remaining structure will fit.  One occasionally confusing side
  effect of this is that two positional arguments for different
  constructors of a sum type can't be composed at the parser level;
  rather, this must be done at the `ReadM` level. For example:

  ```haskell
  import Options.Applicative

  data S3orFile = S3 BucketKey | File FilePath

  s3Read, fileRead :: ReadM S3orFile
  s3Read = S3 <$> ...
  fileRead = File <$> ...

  correct :: Parser S3orFile
  correct = argument (s3Read <|> fileRead) idm

  incorrect :: Parser S3orFile
  incorrect = argument s3Read idm <|> argument fileRead idm
  ```

## How it works
An applicative `Parser` is essentially a heterogeneous list or tree
of `Option`s, implemented with existential types.

All options are therefore known statically (i.e. before parsing,
not necessarily before runtime), and can, for example, be traversed
to generate a help text. Indeed, when displaying the usage text for
a parser, we use an intermediate tree structure.

When we examine the user's input, each argument is examined to
determine if it's an option or flag, or a positional argument. The
parse tree is then searched for a matching term, and if it finds
one, that leaf of the tree is replaced with the value itself. When
all input has been processed, we see if we can generate the complete
value, and if not issue an error.

See [this blog post][blog] for a more detailed explanation based on a
simplified implementation.

 [aeson]: http://hackage.haskell.org/package/aeson
 [applicative]: http://hackage.haskell.org/package/base/docs/Control-Applicative.html
 [arrows]: http://www.haskell.org/arrows/syntax.html
 [attoparsec]: http://hackage.haskell.org/package/attoparsec
 [bash documentation]: http://www.gnu.org/software/bash/manual/html_node/Programmable-Completion-Builtins.html
 [blog]: http://paolocapriotti.com/blog/2012/04/27/applicative-option-parser/
 [hackage]: http://hackage.haskell.org/package/optparse-applicative
 [hackage-png]: http://img.shields.io/hackage/v/optparse-applicative.svg
 [hackage-matrix]: https://matrix.hackage.haskell.org/package/optparse-applicative
 [hackage-matrix-png]: https://matrix.hackage.haskell.org/api/v2/packages/optparse-applicative/badge
 [hackage-deps]: http://packdeps.haskellers.com/reverse/optparse-applicative
 [hackage-deps-png]: https://img.shields.io/hackage-deps/v/optparse-applicative.svg
 [monoid]: http://hackage.haskell.org/package/base/docs/Data-Monoid.html
 [semigroup]: http://hackage.haskell.org/package/base/docs/Data-Semigroup.html
 [parsec]: http://hackage.haskell.org/package/parsec
 [status]: http://travis-ci.org/pcapriotti/optparse-applicative?branch=master
 [status-png]: https://api.travis-ci.org/pcapriotti/optparse-applicative.svg?branch=master
 [ansi-wl-pprint]: http://hackage.haskell.org/package/ansi-wl-pprint
