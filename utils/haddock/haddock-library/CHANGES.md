## Changes in version 1.12.0

  * Backtick spans that are not valid identifiers, e.g. `` `foo bar` ``, are
    now parsed as monospaced text (like `@foo bar@`) instead of plain text.
    The content of such a span is taken literally. As in Markdown, a span
    opened with N backticks closes with N backticks, e.g.
    ``` ``x `plus` y`` ```. A closed span is an identifier only when its
    whole content is one identifier, so `` `foldl' f z xs` `` is code, not
    the identifier `foldl`. (#28090)

  * Backtick spans are not parsed inside `@...@`. An identifier between
    backticks inside `@...@` in an infix application or an operator section,
    as in `` @f `elem` xs@ `` or `` @(`elem` xs)@ ``, is now parsed in infix form (like ``` ``elem`` ```),
    so its backticks are rendered.

## Changes in version 1.11.0

  * Add support for linking identifiers with a quote between backticks (#1408)

## Changes in version 1.10.0

  * Add support for labeled module references (#1360)
## Changes in version 1.9.0

 * Fix build-time regression for `base < 4.7` (#1119)

 * Update parsing to strip whitespace from table cells (#1074)

## Changes in version 1.8.0

 * Support inline markup in markdown-style links (#875)

 * Remove now unused `Documentation.Haddock.Utf8` module.
   This module was anyways copied from the `utf8-string` package.

## Changes in version 1.7.0

 * Make `Documentation.Haddock.Parser.Monad` an internal module

## Changes in version 1.6.1

 * Replace `attoparsec` with `parsec` (#799)

## Changes in version 1.6.0

 * `MetaDoc` stores package name for since annotations

## Changes in version 1.5.0.1

 * Support for parsing unicode operators (#458)

## Changes in version 1.5.0

 * Bifunctor, Bifoldable and Bitraversable instances for DocH and MetaDoc

 * Support for grid tables
   * added `DocTable` constructor to `DocH`
   * added `Table`, `TableCell` and `TableRow` data types
   * added `markupTable` to `DocMarkupH` data type

## Changes in version 1.4.5

 * Move markup related data types to haddock-library
