Parsec [![Build Status](https://travis-ci.org/haskell/parsec.svg?branch=master)](https://travis-ci.org/haskell/parsec)
======

A monadic parser combinator library, written by Daan Leijen. Parsec is designed
from scratch as an industrial-strength parser library. It is simple, safe, well
documented, has extensive libraries, good error messages, and is fast.

Some links:

* [Parsec on Hackage](https://hackage.haskell.org/package/parsec),
  contains the generated documentation.
* The 2001 paper written by Daan Leijen, some what outdated
  ([PDF](https://web.archive.org/web/20140528151730/http://legacy.cs.uu.nl/daan/download/parsec/parsec.pdf),
  [HTML](https://web.archive.org/web/20140528151730/http://legacy.cs.uu.nl/daan/download/parsec/parsec.html),
  thanks to [archive.org](http://web.archive.org);
  and [PDF](https://research.microsoft.com/en-us/um/people/daan/download/parsec/parsec.pdf),
  thanks to Microsoft Research).
* [Using Parsec](http://book.realworldhaskell.org/read/using-parsec.html),
  chapter 16 of [Real World Haskell](http://book.realworldhaskell.org/).
* [An introduction to the Parsec library](http://kunigami.wordpress.com/2014/01/21/an-introduction-to-the-parsec-library)
  on Kunigami's blog.
* [An introduction to parsing text in Haskell with Parsec](http://unbui.lt/#!/post/haskell-parsec-basics) on Wilson's blog.
* Differences between Parsec and
  [Attoparsec](http://hackage.haskell.org/package/attoparsec)
  (Haskell's other prominent parser library) as explained in
  [an answer on StackExchange](http://stackoverflow.com/a/19213247).
* Differences between Parsec and [Happy](http://www.haskell.org/happy)
  (Haskell's parser generator) as explained in two
  answers on separate StackExchange questions
  ([1](http://stackoverflow.com/a/7270904),
  [2](http://stackoverflow.com/a/14775331)).
* Differences between Parsec and
  [Megaparsec](http://hackage.haskell.org/package/megaparsec)
  (an advanced fork of Parsec) as explained in
  [Megaparsec's README](https://github.com/mrkkrp/megaparsec#megaparsec-vs-parsec).


By analyzing [Parsec's reverse dependencies on Hackage](http://packdeps.haskellers.com/reverse/parsec)
we can find open source project that make use of Parsec.  For example
[bibtex](http://hackage.haskell.org/package/bibtex),
[ConfigFile](http://hackage.haskell.org/package/ConfigFile),
[csv](http://hackage.haskell.org/package/csv) and
[hjson](http://hackage.haskell.org/package/hjson).


## Getting started

This requires a working version of `cabal` and `ghci`, which are part of
any modern installation of Haskell, such as
[Haskell Platform](https://www.haskell.org/platform).

First install Parsec.

    cabal install parsec

Below we show how a very simple parser that tests matching parentheses
was made from GHCI (the interactive GHC environment), which we started
with the `ghci` command).

```
Prelude> :m +Text.Parsec
Prelude Text.Parsec> let parenSet = char '(' >> many parenSet >> char ')'
Loading package transformers-0.3.0.0 ... linking ... done.
Loading package array-0.5.0.0 ... linking ... done.
Loading package deepseq-1.3.0.2 ... linking ... done.
Loading package bytestring-0.10.4.0 ... linking ... done.
Loading package mtl-2.1.3.1 ... linking ... done.
Loading package text-1.1.1.3 ... linking ... done.
Loading package parsec-3.1.5 ... linking ... done.
Prelude Text.Parsec> let parens = (many parenSet >> eof) <|> eof
Prelude Text.Parsec> parse parens "" "()"
Right ()
Prelude Text.Parsec> parse parens "" "()(())"
Right ()
Prelude Text.Parsec> parse parens "" "("
Left (line 1, column 2):
unexpected end of input
expecting "(" or ")"
```

The `Right ()` results indicate successes: the parentheses matched.
The `Left [...]` result indicates a parse failure, and is detailed
with an error message.

For a more thorough introduction to Parsec we recommend the links at
the top of this README file.


## Contributing

Issues (bugs, feature requests or otherwise feedback) may be reported in
[the Github issue tracker for this project](https://github.com/haskell/parsec/issues).

Pull-requests are also welcome.


## License

See the [LICENSE](https://github.com/haskell/parsec/blob/master/LICENSE)
file in the repository.
