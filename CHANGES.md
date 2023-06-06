## Changes in 2.28.0
 * `hi-haddock` is integrated, which means docstrings are no longer extracted
   through typchecked module results. Instead, docstrings are taken from Haskell
   interface (`.hi`) files.

 * Support qualified and unqualified names in `--ignore-link-symbol`.

 * Add `--trace-args` flag which prints arguments to standard output. This is
   useful for examining arguments passed when Haddock is invoked through `cabal
   haddock`, as `cabal` uses temporary response files to pass arguments to
   Haddock.

 * Avoid recompilation due to changes in optimization flags.

## Changes in 2.24.0

 * Reify oversaturated data family instances correctly (#1103)

 * Removed the majority of Haddock's possible `panic` routes through
   the TTG refactor to make extension variants empty

## Changes in 2.23.0

 * "Linuwial" is the new default theme (#721, #782, #949)

 * Fix style switcher (enabled by `--built-in-themes`) (#949)

 * Support inline markup in markdown-style links (#875)

 * The hyperlinker backend has been re-engineered to use HIE files
   and display type annotations on expressions (#977)

 * The hyperlinker backend lexer is now more incremental, faster, and
   more memory efficient (#977)

 * Add an "Instances" menu item to the HTML backend for controlling
   settings related to expanding/collapsing instances (#1007)

 * Improved identifier links including value/type namespaces, and
   hyperlinking of parenthesized/backticked identifiers

 * Substantial bugfixes for converting `TyThing` back into source
   declarations (#1003, #1005, #1022, #1020)

 * `--show-interface` now outputs to stdout (instead of stderr)

 * Render associated type defaults and also improve rendering of
   default method signatures

 * Many fixes to the LaTeX backend, mostly focused on not crashing
   as well as generating LaTeX source that compiles

 * More flexible parsing of the module header

## Changes in version 2.22.0

 * Make `--package-version` optional for `--hoogle` (#899)

 * Support type and data families in the LaTeX backend (#734)

 * Support pattern synonyms in the Hoogle backend (#947)

## Changes in version 2.21.0

 * Overhaul handling of data declarations in XHTML and LaTeX. Adds support for
   documenting individual arguments of constructors/patterns (#709)

 * Actually list all fixities for `--hoogle` (#871)

 * Fix broken instance source links (#869)

 * Avoiding line breaks due to ling line in the output of `--hoogle` (#868)

 * Capture docs on type family instances (#867)

## Changes in version 2.20.0

 * Show where instances are defined (#748)

 * `@since` includes package name (#452, #550, #749)

## Changes in version 2.19.0.1

 * Support for linking unicode operators (#458)

 * Hyperlinker: Fix file handle leak in (#763)

## Changes in version 2.19.0

 * Haddock now supports tables in documentation inspired by reSTs grid tables

 * `--quickjump` allows for quick navigation in documentation on hackage and
   other documentation hosting sites.

 * A --reexport flag, which can be used to add extra modules to the
   top-level module tree

 * Haddock no longer reports coverage statistics for hidden modules. By default
   cabal-install marks all package internal modules as hidden.

 * Haddock now writes additional information about the documentation to `meta.json`

 * Fix renaming of type variables after specializing instance method signatures (#613)

 * Move markup related data types to haddock-library

 * Fix: Show empty constraint contexts in pattern type signatures (#663)

 * Fix: Generate constraint signatures for constructors exported as pattern
   synonyms (#663)

 * The hyperlinker backend now uses the GHC lexer instead of a custom one.
   This notably fixes rendering of quasiquotes.

 * Overhaul Haddock's rendering of kind signatures so that invisible kind
   parameters are not printed (#681) (Fixes #544)

 * Recognise `SPDX-License-Identifier` as alias for `License` in module header
   parser (#743)

 * Remove the response file related utilities, and use the ones that
   come with `base` (Trac #13896)

 * Remove the response file related utilities, and use the ones that
   come with `base` (Trac #13896)

## Changes in version 2.18.1

 * Synopsis is working again (#599)

 * Per-argument documentation on class methods working again (#647)

 * Support user defined signatures on pattern synonyms

 * Support for bundled pattern synonyms (#494, #551, #626)

## Changes in version 2.17.4

 * Fix 'internal error: links: UnhelpfulSpan' (#554, #565)

 * Hyperlink backend knows about `DataKinds` (#510)

 * Fix rendering of class methods for `Eq` and `Ord` (#549)

 * Export `MDoc` and `toInstalledIface` from `Haddock.Types`

## Changes in version 2.17.3.1

 * Disable `NFData` instances for GHC types when GHC >= 8.0.2 (#537)

## Changes in version 2.17.3

 * Remove framed view of the HTML documentation

## Changes in version 2.17.2

 * Fix portability of documentation building within GHC

## Changes in version 2.17.1

 * Included with GHC 8.0.1

 * Fixed Makefile for GHC installation

 * Fixed clean rule of GHC Makefile

## Changes in version 2.17.0 (not released)

 * Support typesetting of mathematical expressions via Mathjax (#397)

 * Describe orphan instances defined in a module in its documentation (#449)

 * Produce specialized type signatures for typeclass methods (#425)

 * Support GCC-like response files (#470)

## Changes in version 2.16.2

 * Generate hyperlinked source ourselves (#410, part of GSOC 2015)

 * Fix expansion icon for user-collapsible sections (#412)

 * Break up response file arguments on newlines

 * Various HTML fixes (#301, #406, #407, #421)

 * Line anchors in hyperlinked source (#420)

## Changes in version 2.16.1

 * Don't default to type constructors for out-of-scope names (#253 and
   #375)

 * Fix Hoogle display of constructors (#361)

 * Fully qualify names in Hoogle instances output (#263)

 * Output method documentation in Hoogle backend (#259)

 * Don't print instance safety information in Hoogle (#168)

 * Expand response files in arguments (#285)

 * Build the main executable with -threaded (#399)

 * Use SrcSpan of declarations for inferred type sigs (#207)

 * Fix cross-module instance locations (#383)

 * Fix alignment of Source link for instances in Firefox (#384)

## Changes in version 2.16.0

 * Experimental collapsible header support (#335)

 * Add support for markdown links and images

 * Allow an optional colon after the closing bracket of definition lists.
   This is to disambiguate them from markdown links and will be require with a
   future release.

 * Fix re-exports of built-in type families (#310)

 * Fix parsing of infix identifiers such as ``elem``.

 * Print missing docs by default and add --no-print-missing-docs

 * parser: now parses out some meta data too, breaking the API

 * parser: markdown syntax for images and URLs is now accepted:
   <<foo>> style for images and <foo bar> style for links is now
   considered deprecated. <foo> for links is still OK.

 * parser: add support for @since element: this is paragraph-level
   element of the form ‘@since x.y.z’ where x.y.z is the version
   number. The way it is rendered is subject to change.

 * properly render package ID (not package key) in index (#329)

 * links to source location of class instance definitions

 * Fix code blocks in presence of Windows line endings

 * Deal better with long synopsis lines (#151)

## Changes in version 2.15.0

 * Always read in prologue files as UTF8 (#286 and Cabal #1721)

 * parser: don't wrap headers in DocParagraph (#307)

 * parser: don't mangle append order for nested lists (pandoc #1346)

 * parser: preserve list ordering in certain scenarios (#313)

 * parser: update the attoparsec version used internally giving slight
   parsing performance boost.

 * Move development to be against latest GHC release and not GHC HEAD.

 * Further split up the package to separate the executable from the
   library, necessary by things like GHCJS. We now have
   ‘haddock-library’ which are the parts that don't use GHC API,
   ‘haddock-api’ which are (some of) the parts that do use GHC API and
   ‘haddock’ which merely provides the executable.

 * Export few extra functions in the API.

 * Add compatibility with GHC 7.8.2.

 * Omit unnecessary ‘forall’s (#315 and #86)

 * Remove some files which were really old or did not belong in the
   repository in the first place.

## Changes in version 2.14.3

 * Fix parsing of identifiers with ^ or ⋆ in them (#298)

 * Fix anchors (#308)

## Changes in version 2.14.2

 * Always drop --split-objs GHC flag for performance reasons (#292)

 * Print kind signatures GADTs (#85)

 * Drop single leading whitespace when reasonable from @-style blocks (#201)

 * Fix crashes associated with exporting data family record selectors (#294)

## Changes in version 2.14.1

 * Render * and -> with their UnicodeSyntax equivalents if -U is enabled

 * Display minimal complete definitions for type classes

 * Hide right hand side of TF instances with hidden names on the RHS

## Changes in version 2.14.0

 * Print entities with missing documentation (#258)

 * Print a warning message when given `-optghc` instead of `--optghc` (#5)

 * Add `--compatible-interface-versions` (#231)

 * Allow to generate latex documentation for FFI declarations (#247)

 * Add copyright and license information to generated documentation

 * Improved to Unicode support

 * Bold markup support

 * Nested paragraphs

 * Better escaping

 * Header markup

 * Parser should no longer fail to parse any markup

 * {-# OPTIONS_HADDOCK show-extensions #-} pragma will show the GHC extensions
   enabled in the module.

 * Properly render License field (#271)

 * Print type/data family instances (for exported types only)

 * Fix display of poly-kinded type operators (#189)

 * PatternSynonyms support

 * Fix display of implicit parameters (#260)

 * Fix rendering of Contents when links are present (#276)

 * Fix documentation duplication on record fields (#195)

 * Add `--source-entity-line` for exact line links (eg. things defined
   inside TH splices) (#79)

 * Display fixity information for names with nonstandard fixities

 * Bird tracks specified like "> code" no longer suffer from an extra leading
   space in the code output

## Changes in version 2.13.2

 * Handle HsExplicitListTy in renamer (#213)

 * Allow haddock markup in deprecation messages

 * Export more types from Documentation.Haddock

 * Include everything that is required to run the test suite with the cabal
   package (#230)

## Changes in version 2.13.1

 * Hide instances that are "internal" to a module

 * Add support for properties in documentation

 * Fix a bug with spurious superclass constraints

 * Fix and extend the Haddock API

## Changes in version 2.12.0

 * Labeled URLs (e.g <http://example.net/ some label>)

 * Improved memory usage (new dependency: deepseq)

## Changes in version 2.11.0

 * Show deprecation messages for identifiers

 * List identifiers declared on the same line (with a common type) separately

 * Don't crash on unicode strings in doc comments

 * Fix reporting of modules safe haskell mode

 * Fix a case where we were generating invalid xhtml

 * Improved --qual option (no crashes, proper error messages)

 * A new --qual option "aliased" which qualifies identifiers by the module alias
   used in the source code

 * The Haddock API restores GHC's static flags after invocation

 * Access to unexported identifiers through the Haddock API again

## Changes in version 2.10.0

 * Require GHC >= 7.4

 * Safe Haskell indications on module pages

 * Type declarations on identifiers no longer necessary

 * Add flag --interface-version

 * Warn when comment refers to in-scope identifier without documentation

 * Bug fix: links to out-of-scope things (#78)

 * Bug fix: module references to other packages work again

## Changes in version 2.9.4

  * Require GHC >= 7.2

  * Support Alex 3.x

## Changes in version 2.9.3

  [This is the version that comes with GHC 7.2.1 although it claims it's 2.9.2!]

  * Build with GHC 7.2.1

  * Support blank lines in results of examples

  * A type signature for multiple names generates one signature in the output

  * Minor bug fixes

## Changes in version 2.9.2

  * Build with GHC 7.0.2

  * Write Hoogle output in utf8; fixes GHC build on Windows

## Changes in version 2.9.1

  * Fix build in GHC tree

  * Improve .cabal file

## Changes in version 2.9.0

  * Drop support for ghc < 7

  * New flag --qual for qualification of names

  * Print doc coverage information to stdout when generating docs

  * Include an 'All' option in the A-Z subdivided index

  * Make TOC group header identifiers validate

  * Minor changes to the API

## Changes in the version that comes with GHC 7.0.1

  [This version claims it is 2.8.0 but is actually based on 2.8.1]

  * Fix URL creation on Windows: Use / not \ in URLs.

  * Support GHC 7.0

## Changes in version 2.8.1

  * Fix build on Windows with MinGW

## Changes in version 2.8.0

  * HTML backend completely rewritten to generate semantically rich XHTML
    using the xhtml package.

  * New default CSS based on the color scheme chosen for the new Haskell
    wiki, with a pull-out tab for the synopsis.

  * Theme engine based on CSS files. Themes can be switched from the
    header menu. (New flags --built-in-themes and --theme. The latter
    is an alias for --css which now has extended semantics).

  * Markup support for executable examples/unit-tests. To be used with an
    upcoming version of the DocTest program.

  * Addition of a LaTeX backend.

  * Frames-mode can be enabled from the header menu.

  * Path to source entities can be specified per package, so that source
    links work for cross-package documentation.

  * Support for a second form of enumerated lists (1. 2. etc).

  * Additions and changes to the Haddock API.

  * New flag --no-tmp-comp-dir to tell Haddock to write and pick up
    compilation files (.o, .hi, etc) to/from GHC's output directory instead
    of a temporary directory.

  * Various bug fixes.

-----------------------------------------------------------------------------

## Changes in version 2.6.1 (bug fix release from the stable branch)

  * Fix #128

-----------------------------------------------------------------------------

## Changes in version 2.7.2

  * Add Paths_haddock to library

-----------------------------------------------------------------------------

## Changes in version 2.7.1:

  * Fix problems with library part of .cabal file

-----------------------------------------------------------------------------

## Changes in version 2.7.0:

  * Instances can be commented

  * The Haddock API now exposes more of the internals of Haddock

  * Bug fixes (most importantly #128)

-----------------------------------------------------------------------------

## Changes in version 2.6.0:

  * Drop support for GHC 6.10.*

  * Add support for GHC 6.12.1

  * Cross-package documentation: full documentation show up when re-exporting
    things coming from external packages

  * Lexing and parsing the Haddock comment markup is now done in Haddock
    again, instead of in GHC

  * Slightly prettier printing of instance heads

  * Support platforms for which GHC has no native code generator

  * Add a flag --print-ghc-libdir

  * Minor bug fixes

-----------------------------------------------------------------------------

Changed in version 2.5.0:

  * Drop support for GHC 6.8.*

  * Add support for GHC 6.10.3 and 6.10.4

  * Revert to the old multi-page index for large packages (#106)

  * Show GADT records in the generated documentation

  * Create output directory if it doesn't exist (#104)

  * Use the native codegen instead of compiling via C for TH modules

  * Add --use-unicode flag for displaying prettier versions of common symbols

  * Multiple verbosity levels: remove --verbose and add --verbosity=n

-----------------------------------------------------------------------------

Changed in version 2.4.2:

  * Support GHC 6.10.2

  * Haddock no longer crashes on Template Haskell modules (#68)
    (only with GHC 6.10.2 or above)

  * Fix problem with Template Haskell-generated declarations disappearing (#59)

  * Generate two anchors for each link for compatibility between IE and Opera
    (#45)

  * Improved error messages

  * Show re-exports from external packages again (GHC ticket #2746)

  * Store hidden modules in .haddock files again (needed by the haddock
    library)

  * Avoid processing boot modules

  * Pragmas may exist between document comments and declarations

  * Do not indicate that a constructor argument is unboxed

  * Fix problem with non-working links to ghc-prim

  * Allow referring to a specific section within a module in a module link
    (#65)

  * Fixes to the Hoogle backend

  * Improvements to the haddock library

  * Many other fixes (including #67, #69, #58, #57)

-----------------------------------------------------------------------------

Changed in version 2.4.1:

  * Depend on base 4.* when GHC >= 6.9, otherwise 3.*

-----------------------------------------------------------------------------

Changed in version 2.4.0:

  * Add framed view of the HTML documentation

  * Build with GHC 6.8.2 and 6.8.3 again

  * Support images in documentation comments again

  * Small improvements to the Hoogle output

  * A few bugs has been fixed

-----------------------------------------------------------------------------

Changed in version 2.3.0:

  * Support for GHC 6.10.1

  * Slightly improved space usage

  * Fix a bug that made hidden modules show up in the contents & index pages

  * Fix a bug that made Haddock load modules twice

  * Improvements to the Hoogle output

-----------------------------------------------------------------------------

Changed in version 2.2.2:

  * Relax version dependency on ghc-paths

-----------------------------------------------------------------------------

## Changes in version 2.2.1:

  * Support for GHC 6.8.3

  * The Hoogle backend is back, thanks to Neil Mitchell. The plan is to be
    compatible with the upcoming Hoogle 4 pre-release

  * Show associated types in the documentation for class declarations

  * Show type family declarations

  * Show type equality predicates

  * Major bug fixes (#1 and #44)

  * It is no longer required to specify the path to GHC's lib dir

  * Remove unnecessary parenthesis in type signatures

-----------------------------------------------------------------------------

## Changes in version 2.1.0:

  * Fix a bug that made links point to the defining module instead
    of the "best" one (e.g Int pointing to GHC.Base instead of Data.Int)

  * Fix a couple of smaller bugs

  * The representation of DocName was changed in the library

  * Add a flag --no-warnings for turning off warnings

-----------------------------------------------------------------------------

## Changes in version 2.0.0.0:

  * The GHC API is used as the front-end

  * Haddock now understands all syntax understood by GHC 6.8.2

  * Haddock can generate documentation for some of the language extensions
    in GHC 6.8.2

  * Format of module attributes has changed. The only way of specifying
    module attributes is via a new OPTIONS_HADDOCK pragma. Example:
    {-# OPTIONS_HADDOCK hide, prune #-}

  * Haddock understands literate source files

  * Add a small library to read Haddock's interface files

  * Add a flag -B for passing the path to the GHC lib dir

  * Add a flag --optghc for passing options to GHC

  * Add a flag --ghc-version for printing the GHC version

  * Remove --use-package, --allow-missing-html, --ghc-pkg, in favour of only
    supporting --read-interface

  * Remove --package flag, the GHC flag -package-name can be used instead

  * Remove --no-implicit-prelude flag, the GHC flag -XNoImplicitPrelude can
    be used instead

-----------------------------------------------------------------------------

## Changes in version 0.9:

  * Infix type operators, e.g., first :: a~>a' -> (a,b)~>(a',b)

  * Add a search box to the index page which automatically narrows
    the index to the search as you type (thanks to Neil Mitchell).

  * Add a --ghc-pkg flag

  * Add a flag --allow-missing-html

  * URL expansion for %%, %L, %{LINE}

  * added substitution %{FILE///c}

  * Lexing of /.../ is now more like '...', in that a / character must
    be matched by another / character on the same line, otherwise it
    is treated as a literal '/'.  This removes a common source of
    accidental parse errors in documentation.

  * Various bugs were fixed.

  * Cabal 1.2 is now required, and Haddock builds with GHC 6.8.x.

-----------------------------------------------------------------------------

## Changes in version 0.8:

  * Haddock has a Cabal build system, and will build on Windows without
    Cygwin or MSYS.  GHC 6.4 or later is required.

  * New options: --comments-base, --comments-module, --comments-entity
    for generating links to pages (eg. wiki) for collecting user comments.

  * New options: --source-base, --source-module, --source-entity
    for generating links to source code.  Haddock now understands
    {-# LINE #-} pragmas, which means it can figure out the correct
    name for the source file even if the file is preprocessed.

  * Haddock generates output for populating the Hoogle search engine.

  * New markup <<url>> for including images.

-----------------------------------------------------------------------------

## Changes in version 0.7:

  * ## Changes in the way Haddock decides where to direct hyperlinks.  Each entity
    is now assigned a single "home" module, and all hyperlinks are directed
    to that module.  See the docs ("Hyperlinking and re-exported entities")
    for details.

  * New options --ignore-all-exports, --hide

  * New option --use-package for creating documentation that hyperlinks to
    the HTML documentation for existing packages on your system.  It works
    by querying ghc-pkg for the location of the Haddock interface and
    HTML for the given package.

  * Parts of the HTML documentation, notably lists of instances, are
    now "collapsible" with a +/- button.  Also, the contents page is now
    in the form of tree in which subtrees can be collapsed.

  * Support for Microsoft DevHelp and HtmlHelp 2.0 formats.

  * Support for a short description for each module for the contents page.

  * Compiles with GHC 6.4+

  * Various bugfixes

-----------------------------------------------------------------------------

## Changes in version 0.6:

  * Implicit parameters, zip comprehensions and 'mdo' are now
    supported by the parser.

  * Some lexical bugs were fixed, mainly concerning literal paragraphs
    next to non-literal paragraphs.

  * New options:  --gen-index, --use-index, --gen-contents and --use-contents,
    for generting a combined index and contents for several libraries
    from the appropriate .haddock files.

  * There is now one index rather than separate Function/Constructor and
    Type/Class indices.  Where a name refers to several entities, these
    are listed separately in the index.

  * New option: -v, elicits more verbose warnings.  Some warnings are
    now turned off by default; Haddock should be a little less noisy
    in general.

  * Markup for definition lists has been added.  See the documentation
    for details.

  * New option: --package for setting the package name.  The package
    is listed alongside each module name in the combined contents.

  * Entities which are re-exported from another package now at least
    leave a hyperlink in the generated HTML, rather than nothing at all.

  * Some fixes for bugs which caused incorrect hyperlinks in the
    generated HTML.  In particular, instances should now be linked
    correctly.

  * Some aesthetic improvements to the generated HTML.

-----------------------------------------------------------------------------

## Changes in version 0.5:

  * Compiles with recent versions of GHC.

  * A few bugs have been fixed.

  * New labelling/linking feature (see "Anchors" in the manual).

-----------------------------------------------------------------------------

## Changes in version 0.4:

  * Import declarations which list entities in parentheses, or with
    hiding clauses, are now properly supported.  Modulo one or two
    small infelicities, the full Haskell modules system is now supported.

  * Haddock is now more flexible about the placing of separators
    (commas, semicolons) in relation to doc comments.

  * There is support for generating and reading "interface files"
    which describe the exports of a set of modules.  This is useful
    for generating documentation which hyperlinks to
    previously-generated documentation.

  * Support for generating the extra files required by the Microsoft
    Help compiler.

  * New option: --prologue specifies a file of documentation which is
    placed on the contents page.

  * Many bugs fixed

-----------------------------------------------------------------------------
## Changes in version 0.3:

  * Documentation on individual function arguments is now implemented

  * Links can be made to identifiers that aren't in scope, by using
    the fully qualified name.

  * Instances are collected and listed with the appropriate classes
    and datatypes (not for derived instances yet).

  * Single quotes are only interpreted specially when they surround a
    legal Haskell identifier, otherwise they are treated literally.

  * Bird-tracked text is now literal.  If you want marked-up text in
    a code block, use the @...@ notation instead.

  * Various changes to the layout, it generally looks nicer now.

  * Various bugs fixed.

-----------------------------------------------------------------------------
## Changes in version 0.2:

  * The [...] markup for typewriter font has been changed to @...@.

  * Module attributes introduced (see the documentation for details).

  * {- ... -} style comments may now be used for documentation annotations

  * Recursive modules are detected and reported as an error.

  * Existential constructors and rank-N types are now groked by the parser

  * Some type rendering cleaned up

  * `abc' is accepted as well as 'abc' to markup an identifier

  * Several bugs fixed, and error messages improved.
