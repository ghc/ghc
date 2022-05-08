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
