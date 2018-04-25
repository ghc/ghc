
-- This one killed GHC 5.02 with a funResultTy panic
-- The reason was that the simplifier was doing a 
-- case-of-case where the result had a polymorphic type.
-- This in turn showed up because of a newtype (now 
-- transparent) with a forall inside it.
--
-- It's quite hard to tickle this one, hence the two-module setup.

module FormParse where
  
import Control.Monad
import Simpl009Help

identifier :: Parser Char Char
identifier =
  do c <- lookAhead
     guard (c == 'a')
     return c




