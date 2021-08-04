## Version 0.16.1.0 (21 Nov 2020)

- Guard `process` dependency behind an on by default flag.
  This allows one to shrink the dependency tree significantly
  by turning off the ability to use bash completion actions.

- Remove `bytestring` dependency from the test suite.

## Version 0.16.0.0 (14 Aug 2020)

- Add `Options.Applicative.NonEmpty.some1` function, which
  parses options the same as `some1` from base, but doesn't
  cause duplicates in the usage texts.

- Further improve help text generation in the presence
  of optional values when nesting is involved, and many and
  some when displayed with a suffix.

- Add "global" options to the usage texts for subcommands.
  When using subcommands, a "global options" section can
  now appear below the options and commands sections.

  Global options are *off* by default, to enable them, use
  the `helpShowGlobals` modifier.

  The `noGlobal` builder will suppress a single option being
  displayed in the global options list.

  Fixes issues:
    * \# 175 - List detailed subparser documentation with `--help`
    * \# 294 - Displaying global options when listing options for a command.
    * \# 359 - Subcommand help text lacks required parent command arguments

- Allow the `--help` option to take the name of a command.
  Usage without any arguments is the same, but now, when an
  argument is given, if it is the name of a currently
  reachable command, the help text for that command will
  be show.

  Fixes issues:
    * \# 379 - cmd --help subcmd is not the same as cmd subcmd --help

- Updated dependency bounds.

- Add builder for the all positional parser policy.

- Remove deprecated functions
    * nullOption
    * execParserMaybe
    * customExecParserMaybe

## Version 0.15.1.0 (12 Sep 2019)

- Improve printing of brief descriptions for parsers.
  Previously, the logical structure of the parser,
  such as alternative groups and segments which must
  be defined together, did not influence the layout of
  the brief description. This could lead to some help
  texts being difficult to read.  Now, we use nesting
  and forced line breaks to help improve readability.

## Version 0.15.0.0 (05 Jul 2019)

- Add support for GHC 8.8.1.

- Add `subparserInline` modifier as additional way of
  executing subparsers. When activated, the subparser
  parse tree will be inserted into that of the parent
  instead of being run independently, allowing mixing
  of child and parent options.

- Improve rendering of complex nested parse structures.
  Previously, brackets and parenthesis did not respect
  whether or not options had to be defined together.
  Now the parse tree is more accurately represented in
  the help text.

- Add `helpLongEquals` modifier, which will change how
  long options are printed in the help text, adding an
  equals sign, for example "--input=FILE".

- Updated dependency bounds.

- Clean ups and Documentation.

## Version 0.14.3.0 (03 Oct 2018)

- Updated dependency bounds.

- Fix tab completion with Commands being unreachable.

- Clean ups and Documentation.

## Version 0.14.2.0 (26 Feb 2018)

- Updated dependency bounds.

## Version 0.14.1.0 (23 Feb 2018)

- Updated dependency bounds.

- Export `HasName`, `HasCompleter`, `HasValue`, and
  `HasMetavar` type classes.

- Doc.

## Version 0.14.0.0 (09 Jun 2017)

- Upgrade `str` and related builders to be polymorphic over
  `IsString`. This allows `Text` and `Bytestring` to be used
  naturally with `strOption` and `strArgument` and friends.

  *Note:* This change may require additional type signatures
          in cases where the reader was necessary for type
          inference.

- Export public API explicitly from `Options.Applicative`
  instead of re-exporting other modules.

  *Note:* Minor changes to exports were made in conjunction
          to this change. `ParserHelp` no longer requires an
          an extra import, and some internally used functions
          from `Options.Applicative.Common` are no longer
          exported from the main module.

- Add Zsh and Fish completions with rich descriptions for
  options and commands.

  Use "--zsh-completion-script" and "fish-completion-script"
  to generate scripts for these shells.

- Fix bash completions with quoted sections, tilde expansions
  and completions after "--".

- Add suggestions to error message when a user mistypes a
  command or option.

- Add `style` builder, for styling option descriptions.

- Improve error message for options when a required argument
  is not supplied.

- Fix #242 regarding flags with long options, where a flag given
  a long option could be interpreted incorrectly.

- Fix `noIntersperse` to be more like its namesakes in other
  libraries. When on, options will be accepted until an argument
  is passed, after which all options will be treated as positional
  arguments.

- Add `forwardOptions` builder, which will allow unknown options
  and flags to be passed to an argument builder.
  This is useful to mixed parsing environments, or wrappers to
  other commands.

- Add `Semigroup` instances for `Completer` and `Chunk`.

- Forwards compatibility with `MonadFail` proposal.

- Doc

## Version 0.13.2.0 (9 Mar 2017)

- Updated dependency bounds.

- Doc

## Version 0.13.1.0 (10 Feb 2017)

- Updated dependency bounds.

- Add required test files to cabal package.

- Doc

## Version 0.13.0.0 (15 Aug 2016)

- Implement command groups, which allow subcommands to have their own
  usage description.

- Implement showHelpOnEmpty, which is similar to showHelpOnError, but only
  fires when a command or subcommand is begun, and suppresses the "Missing:"
  error text.

- Fix ghc 8.0 warnings.

- Fix ghc 7.10 warnings.

- Bump dependency bounds.

- Add maybeReader function for convenient ReadM creation.

- Move eitherReader to Readers section (for better discoverability).

- Fix hsubparser metavar override.

- Remove ComplError, which was dead code.

- Reimplement Missing error generation, which overly complicated evalParser.

- Export Semigroup instances for types which are also Monoids. Removes
  mempty synonym `(<>)` export, as it clashes with Semigroup exports.
  One may need to import Data.Monoid or Data.Semigroup when upgrading.

- Use a Cabal test suite for tests, simplify test dependencies.

## Version 0.12.1.0 (18 Jan 2016)

- Updated dependency bounds.

- Improve subparser contexts to improve usage error texts.

- Docs.

- Fixed bugs
    * \# 164 - Invalid options and invalid arguments after parser has succeeded
               not displaying
    * \# 146 - multi-word filename completion is broken


## Version 0.12.0.0 (17 Sep 2015)

- Add "missing" error condition descriptions when required flags and arguments
  are not provided.

- Allow multiple short flags to be concatenated together behind a single
  hyphen, e.g. "-xcf".

- Updated dependency bounds on `process` and `ansi-wl-pprint`.

- Add `Show` and `Eq` instances to some types for easier debugging.

- Add defaultPrefs, a default preferences value.

- Docs.

## Version 0.11.0.2 (17 Feb 2015)

- Updated dependency bounds.

## Version 0.11.0.1 (5 Oct 2014)

- Updated documentation.

## Version 0.11.0 (4 Oct 2014)

- Added Alternative instances for `Chunk` and `ReadM`.

- The `ReadM` monad is now a `ReaderT` for the argument being parsed.  User
  defined readers do not need to handle their argument explicitly, but can
  always access it using `readerAsk`.

- Argument builders now take a `ReadM` parameter, just like options.

- Fixed bugs
    * \#106 - argument should perhaps use `ReadM`

## Version 0.10.0 (1 Sep 2014)

- Parser execution and help text generation are now more modular, and allow for
  greater customisation.

- More consistent API for `option` and `argument` builders: now `option` takes
  a reader as argument, and `nullOption` is deprecated in favour of `option`.
  The `reader` modifier is gone.  Quick migration guide:

    * `option` (without a `reader` modifier) => `option auto`
    * `nullOption` (without a `reader` modifier) => `option disabled`
    * `option`/`nullOption` (with a `reader r` modifier) => `option r`.

- Added convenience builder `strArgument`, equivalent to `argument str`.

- Removed functions deprecated from at least version 0.8.0.

- Switched test infrastructure to `tasty`.

- Fixed bugs
    * \#63 - Inconsistency between 'argument' and 'strOption' types

## Version 0.9.1.1 (31 Jul 2014)

- Fixed bugs
    * \#97 - Version 0.9.1 fails test suite

## Version 0.9.1 (30 Jul 2014)

- Documentation tweaks.

- Added low-level function to handle parse results (pull request \#94).

- `ParserResult` now has a `Show` instance (see issue \#95).

- Fixed bugs
    * \#93 - Formatting problem for several sub-parsers

## Version 0.9.0 (23 May 2014)

- The option returned by `abortOption` is now visible by default.

## Version 0.8.1 (5 May 2014)

- Fixed bugs
    * \#74 - Missing newline

## Version 0.8.0.1 (19 Mar 2014)

- Fixed bugs
    * \#73 - Release 0.8.0 is broken

## Version 0.8.0 (16 Mar 2014)

- Help page formatting.  Added `columns` preference modifier,
  which can be used to specify the number of columns in the output
  terminal.

- Deprecated `arguments` and `arguments1` builders. Using `many` and `some` on a
  parser built using `argument` now returns a multiple argument parsers that
  behaves correctly with respect to `--`.

- Fixed bugs
    * \#60 - runParser can't be called
    * \#64 - --help behaviour

## Version 0.7.0.2 (18 Oct 2013)

- Fixed bugs
    * \#51 - Build fails with ghc 6.12.3 and ghc 7.0.4

## Version 0.7.0.1 (18 Oct 2013)

- Minor docs fixes.

## Version 0.7.0 (17 Oct 2013)

- Added builders for options that always fail. This makes it
  easier to create options that just print an error message or
  display some brief information and then exit (like `--version`).

- Added `execParserMaybe` and `customExecParserMaybe` functions
  (pull request #49).

- Fixed bugs
    * \#47 - Current master prints help text instead of error
    * \#48 - Can we have an eitherReader convenience function?
    * \#50 - In order parsing problems.
    * \#22 - Strict (no-intersperse) arguments

## Version 0.6.0 (11 Oct 2013)

- Arguments are now always parsed in order.

- Fixed bugs
    * \#40 - Add context information to error messages
    * \#41 - Readme uses old reader API
    * \#38 - Internal types leaking into public API
    * \#44 - Can the build input restriction process == 1.1.* be relaxed?
    * \#28 - Help for subcommands

## Version 0.5.2.1 (24 Dec 2012)

- Minor docs fixes.

## Version 0.5.2 (23 Dec 2012)

- Fixed compatibility with GHC 7.2.

## Version 0.5.1 (23 Dec 2012)

- There is a new parser preference `noBacktrack`, that controls whether how a
  failure in a subparser is propagated. By default, an unknown option in a
  subparser causes the option to be looked up in parent parsers. When
  `noBacktrack` is used, this behavior is disabled. This is useful to implement
  subcommands that have no relations with their parent commands.

- Fixed bugs
    * \#35 - Artifacts of "hidden"
    * \#31 - Backtracking on commands
    * \#25 - Allow for using Maybe in options types to specify optional arguments
    * \#34 - No simple/obvious way to add a --version switch
    * \#29 - Document Mod
    * \#26 - Improve docs for the `Arrow` interface

## Version 0.5.0 (22 Dec 2012)

- Fewer GHC extensions required.

- Improved error handling: unrecognized options now result in an error message.

- By default, the full help text is not displayed on parse errors anymore.
  This behavior can be controlled with the `prefShowHelpOnError` field of
  `ParserPrefs`.

- The `(&)` operator is now deprecated. Modifiers can still be combined using
  `(<>)` or `mappend`.

- Fixed bugs
    * \#37 - Use (\<\>) instead of (&) in documentation

## Version 0.4.3 (09 Dec 2012)

- Updated dependency bounds.

## Version 0.4.2 (26 Nov 2012)

- Fixed bugs
    * \#27 - Please include the test source files in the cabal sdist tarball

## Version 0.4.1 (04 Sep 2012)

- Fixed bugs
    * \#19 - Regression

## Version 0.4.0 (05 Aug 2012)

- Brief help text for nested commands now shows the full command line.

- Fixed inefficiency in the `arguments` parsers for long argument lists.

- Added automatic [bash
completion](https://github.com/pcapriotti/optparse-applicative/wiki/Bash-Completion).

- Added `disambiguate` modifier for `prefs`, which enabled automatic
disambiguation of option abbreviations. With disambiguation on, a command line
like:

        foo --out

    will match an option called `--output`, as long as its the only one starting
    with the string `out`.

- Added `briefDesc` modifier.

- Fixed bugs
    * \#8 - Long options not disambiguated
    * \#10 - Shell completions
    * \#16 - Possible memory leak?

## Version 0.3.2 (31 Jul 2012)

- Fixed bug where both branches of an alternative could be matched.

- Improved brief help text for alternatives.

## Version 0.3.1 (30 Jul 2012)

- Added new `showDefault` and `showDefaultWith` modifiers, which will result in
the default value (if present) to be displayed in the help text.

- Fixed bugs
    * \#12 - Optionally display default values in help

## Version 0.3.0 (30 Jul 2012)

- Option modifiers are now instances of `Monoid` instead of `Category`.

- Dropped dependencies on data-default and data-lens.

- Fixed bugs
    * \#14 - "arguments" can no longer take a list as a default

## Version 0.2.0 (23 Jul 2012)

- Parser is now an instance of Alternative. This makes it possible to build
certain complex parsers that were not definable before. See
`tests/Examples/Alternatives.hs` for a simple example.

- Removed `multi` modifier. You can now use the `many` or `some` methods from
`Alternative`, instead, to create parsers for options that can appear more than
once.

- Added new `flag'` builder that returns a flag without a default value.
Although flags without default values were not useful before, with the addition
of `Alternative` combinators, they do have valid use cases.

- Added new `internal` modifier for options. An internal option is completely
invisible in the help text.

- Added a new `customExecParser` function, which takes an additional
`ParserPrefs` parameter. At the moment, `ParserPrefs` can only be used to
control how many-valued option metavars are displayed in the help text. Setting
its `multiSuffix` field to e.g. `...` will result in an `arguments` parser
description like `[METAVAR]...`.

- Fixed bugs
    * \#6 - "arguments" swallows options
    * \#5 - Help formatting for "arguments" misleading

## Version 0.1.1 (21 Jul 2012)

- New arrow interface.

- Fixed bugs
    * \#7 - "arguments" reads positional arguments in reverse

## Version 0.1.0 (07 Jul 2012)

- Improved error reporting internals.

- Removed template-haskell dependency.

- Fixed bugs:
    * \#3 - No help for subparsers
    * \#4 - Extra empty lines around command list

## Version 0.0.1 (09 Jun 2012)

- Initial release.
