-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Strings
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- "Scrap your boilerplate" --- Generic programming in Haskell 
-- See <http://www.cs.vu.nl/boilerplate/>.
--
-----------------------------------------------------------------------------

module Data.Generics.Strings ( 

	-- * Generic operations for string representations of terms
	gshow,
	gread

 ) where

------------------------------------------------------------------------------

import Control.Monad
import Data.Maybe
import Data.Typeable
import Data.Generics.Basics
import Data.Generics.Aliases



-- | Generic show: an alternative to \"deriving Show\"
gshow :: Data a => a -> String

-- This is a prefix-show using surrounding "(" and ")",
-- where we recurse into subterms with gmapQ.
-- 
gshow = ( \t ->
                "("
             ++ conString (toConstr t)
             ++ concat (gmapL ((++) " " . gshow) t)
             ++ ")"
        ) `extQ` (show :: String -> String)


-- | The type constructor for gunfold a la ReadS from the Prelude;
--   we don't use lists here for simplicity but only maybes.
--
newtype GRead a = GRead (String -> Maybe (a, String))
unGRead (GRead x) = x

{-
-- | Turn GRead into a monad.
instance Monad GRead where
  return x = GRead (\s -> Just (x, s))
  (GRead f) >>= g = GRead (\s -> 
                             maybe Nothing 
                                   (\(a,s') -> unGRead (g a) s')
                                   f s
                          )
-}

-- | Generic read: an alternative to \"deriving Read\"
gread :: GenericB Maybe

{-

This is a read operation which insists on prefix notation.  (The
Haskell 98 read deals with infix operators as well. We will be able to
deal with such special cases as well as sonn as we include fixity
information into the definition of "Constr".)  We use gunfold to
"parse" the input. To be precise, gunfold is used for all result types
except String. The type-specific case for String uses basic String
read. Another source of customisation would be to properly deal with
infix operators subject to the capture of that information in the
definition of Constr. The "gread" combinator properly checks the 
validity of constructors before invoking gunfold in order to rule
out run-time errors.

-}

gread = undefined

{-
gdefault `extB` scase

 where

  -- a specific case for strings
  scase s = case reads s of
              [x::(String,String)] -> Just x
              _ -> Nothing

  -- the generic default of gread
  gdefault s = undefined

-}

{-
    do s' <- return $ dropWhile ((==) ' ') s
       guard (not (s' == ""))
       guard (head s' == '(')
       (c,s'')  <- prefixConstr (dropWhile ((==) ' ') (tail s'))
       u  <- return undefined 
       dt <- return $ dataTypeOf u
       case stringCon dt c of
        Nothing -> error "Generics: gread failed"
        Just c' -> 
          gunfoldm c' gread

       guard ( or [ maxConIndex (dataTypeOf u) == 0
                  , c `elem` constrsOf u
                  ]
             )
       (a,s''') <- unGRead (gunfold f z c) s''
       _ <- return $ constrainTypes a u
       guard (not (s''' == "")) 
       guard (head s''' == ')')
       return (a, tail s''')


  -- To force two types to be the same
  constrainTypes :: a -> a -> ()
  constrainTypes _ _ = ()

  -- Argument f for unfolding
  f :: Data a => GRead (a -> b) -> GRead b
  f x = GRead (\s -> do (r,s') <- unGRead x s
                        (t,s'')  <- gread s'
                        return (r t,s''))

  -- Argument z for unfolding
  z ::  forall g. g -> GRead g
  z g = GRead (\s -> return (g,s))


  -- Get Constr at front of string
  prefixConstr :: String -> Maybe (Constr, String)

  -- Assume an infix operators in parantheses
  prefixConstr ('(':s)
    = case break ((==) ')') s of
        (s'@(_:_),(')':s'')) -> Just (Constr ("(" ++ s' ++ ")"), s'')
        _ -> Nothing

  -- Special treatment of multiple token constructors
  prefixConstr ('[':']':s) = Just (Constr "[]",s)

  -- Try lex for ordinary constructor and basic datatypes
  prefixConstr s
    = case lex s of
        [(s'@(_:_),s'')] -> Just (Constr s',s'')
        _ -> Nothing

-}