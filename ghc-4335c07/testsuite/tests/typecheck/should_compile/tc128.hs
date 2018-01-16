-- !!! Test type checking of mutually recursive groups
-- GHC 5.00 was falling into a black hole when type checking a recursive
-- group of type declarations including a *chain* of type synonyms.

module ShouldCompile where

  type PhraseFun = PMap -> Float
  type PMap      = () -> Player
  data Player    = MkT PhraseFun

