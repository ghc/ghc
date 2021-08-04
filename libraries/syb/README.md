syb: Scrap Your Boilerplate!
================================================================================

Scrap Your Boilerplate (SYB) is a library for generic programming in Haskell. It 
is supported since the GHC >= 6.0 implementation of Haskell. Using this 
approach, you can write generic functions such as traversal schemes (e.g., 
everywhere and everything), as well as generic read, generic show and generic 
equality (i.e., gread, gshow, and geq). This approach is based on just a few 
primitives for type-safe cast and processing constructor applications. 

It was originally developed by Ralf Lämmel and Simon Peyton Jones. Since then,
many people have contributed with research relating to SYB or its applications. 

More information is available on the webpage: 
http://www.cs.uu.nl/wiki/GenericProgramming/SYB


Features
--------

* Easy generic programming with combinators
* GHC can derive Data and Typeable instances for your datatypes
* Comes with many useful generic functions


Requirements
------------

* GHC 6.10.1 or later
* Cabal 1.6 or later


Bugs & Support
--------------

Please report issues or request features at the bug tracker:

  https://github.com/dreixel/syb/issues

For discussion about the library with the authors, maintainers, and other
interested persons use the mailing list:

  http://www.haskell.org/mailman/listinfo/generics
