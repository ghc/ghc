0.14.1

* Added `Data.Attoparsec.ByteString.getChunk`.

0.14.0

* Added `Data.Attoparsec.ByteString.takeWhileIncluding`.
* Make `Data.Attoparsec.{Text,ByteString}.Lazy.parseOnly` accept lazy input.

0.13.2.1

* Improved performance of `Data.Attoparsec.Text.asciiCI`

0.13.2.0

* `pure` is now strict in `Position`

0.13.1.0

* `runScanner` now correctly returns the final state
  (https://github.com/bos/attoparsec/issues/105).
* `Parser`, `ZeptoT`, `Buffer`, and `More` now expose `Semigroup` instances.
* `Parser`, and `ZeptoT` now expose `MonadFail` instances.

0.13.0.2

* Restore the fast specialised character set implementation for Text
* Move testsuite from test-framework to tasty
* Performance optimization of takeWhile and takeWhile1

0.13.0.1

* Fixed a bug in the implementations of inClass and notInClass for
  Text (https://github.com/bos/attoparsec/issues/103)

0.13.0.0

* Made the parser type in the Zepto module a monad transformer
  (needed by aeson's string unescaping parser).

0.12.1.6

* Fixed a case folding bug in the ByteString version of stringCI.

0.12.1.5

* Fixed an indexing bug in the new Text implementation of string,
  reported by Michel Boucey.

0.12.1.4

* Fixed a case where the string parser would consume an unnecessary
  amount of input before failing a match, when it could bail much
  earlier (https://github.com/bos/attoparsec/issues/97)

* Added more context to error messages
  (https://github.com/bos/attoparsec/pull/79)

0.12.1.3

* Fixed incorrect tracking of Text lengths
  (https://github.com/bos/attoparsec/issues/80)

0.12.1.2

* Fixed the incorrect tracking of capacity if the initial buffer was
  empty (https://github.com/bos/attoparsec/issues/75)

0.12.1.1

* Fixed a data corruption bug that occurred under some circumstances
  if a buffer grew after prompting for more input
  (https://github.com/bos/attoparsec/issues/74)

0.12.1.0

* Now compatible with GHC 7.9

* Reintroduced the Chunk class, used by the parsers package

0.12.0.0

* A new internal representation makes almost all real-world parsers
  faster, sometimes by big margins.  For example, parsing JSON data
  with aeson is now up to 70% faster.  These performance improvements
  also come with reduced memory consumption and some new capabilities.

* The new match combinator gives both the result of a parse and the
  input that it matched.

* The test suite has doubled in size.  This made it possible to switch
  to the new internal representation with a decent degree of
  confidence that everything was more or less working.

* The benchmark suite now contains a small family of benchmarks taken
  from real-world uses of attoparsec.

* A few types that ought to have been private now are.

* A few obsolete modules and functions have been marked as deprecated.
  They will be removed from the next major release.

0.11.3.0

* New function scientific is compatible with rational, but parses
  integers more efficiently (https://github.com/bos/aeson/issues/198)

0.11.2.0

* The new Chunk typeclass allows for some code sharing with Ed
  Kmett's parsers package: http://hackage.haskell.org/package/parsers

* New function runScanner generalises scan to return the final state
  of the scanner as well as the input consumed.


0.11.1.0

* New dependency: the scientific package.  This allows us to parse
  numbers much more efficiently.

* peekWord8', peekChar': new primitive parsers that allow
  single-character lookahead.
