-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.Parsec.Perm
-- Copyright   :  (c) Daan Leijen 1999-2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  daan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  non-portable (uses existentially quantified data constructors)
--
-- This module implements permutation parsers. The algorithm used
-- is fairly complex since we push the type system to its limits :-)
-- The algorithm is described in:
--
-- /Parsing Permutation Phrases,/
-- by Arthur Baars, Andres Loh and Doaitse Swierstra.
-- Published as a functional pearl at the Haskell Workshop 2001.
-- 
-----------------------------------------------------------------------------

module Text.ParserCombinators.Parsec.Perm
                  ( PermParser  -- abstract

                  , permute
                  , (<||>), (<$$>)
                  , (<|?>), (<$?>)
                  ) where

import Text.ParserCombinators.Parsec

{---------------------------------------------------------------

---------------------------------------------------------------}
infixl 1 <||>, <|?>
infixl 2 <$$>, <$?>


{---------------------------------------------------------------
  test -- parse a permutation of 
  * an optional string of 'a's
  * a required 'b'
  * an optional 'c'
---------------------------------------------------------------}
test input
  = parse (do{ x <- ptest; eof; return x }) "" input

ptest :: Parser (String,Char,Char)
ptest  
  = permute $
    (,,) <$?> ("",many1 (char 'a'))
         <||> char 'b' 
         <|?> ('_',char 'c')


{---------------------------------------------------------------
  Building a permutation parser
---------------------------------------------------------------}
(<||>) :: PermParser tok st (a -> b) -> GenParser tok st a -> PermParser tok st b
(<||>) perm p     = add perm p                  
(<$$>) f p        = newperm f <||> p

(<|?>) perm (x,p) = addopt perm x p
(<$?>) f (x,p)    = newperm f <|?> (x,p)



{---------------------------------------------------------------
  The permutation tree
---------------------------------------------------------------}
data PermParser tok st a = Perm (Maybe a) [Branch tok st a]
data Branch tok st a     = forall b. Branch (PermParser tok st (b -> a)) (GenParser tok st b)


-- transform a permutation tree into a normal parser
permute :: PermParser tok st a -> GenParser tok st a
permute (Perm def xs)
  = choice (map branch xs ++ empty)
  where
    empty
      = case def of
          Nothing -> []
          Just x  -> [return x]

    branch (Branch perm p)
      = do{ x <- p
          ; f <- permute perm
          ; return (f x)
          }

-- build permutation trees
newperm :: (a -> b) -> PermParser tok st (a -> b)
newperm f
  = Perm (Just f) []

add :: PermParser tok st (a -> b) -> GenParser tok st a -> PermParser tok st b
add perm@(Perm mf fs) p
  = Perm Nothing (first:map insert fs)
  where
    first   = Branch perm p
    insert (Branch perm' p')
            = Branch (add (mapPerms flip perm') p) p'

addopt :: PermParser tok st (a -> b) -> a -> GenParser tok st a -> PermParser tok st b
addopt perm@(Perm mf fs) x p
  = Perm (fmap ($x) mf) (first:map insert fs)
  where
    first   = Branch perm p
    insert (Branch perm' p')
            = Branch (addopt (mapPerms flip perm') x p) p'


mapPerms :: (a -> b) -> PermParser tok st a -> PermParser tok st b
mapPerms f (Perm x xs)
  = Perm (fmap f x) (map (mapBranch f) xs)
  where
    mapBranch f (Branch perm p)
      = Branch (mapPerms (f.) perm) p
