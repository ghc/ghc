-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Text
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Generics.Basics)
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell 
-- See <http://www.cs.vu.nl/boilerplate/>. The present module provides
-- generic operations for text serialisation of terms.
--
-----------------------------------------------------------------------------

module Data.Generics.Text ( 

	gshow,
	gread

 ) where

------------------------------------------------------------------------------

#ifdef __HADDOCK__
import Prelude
#endif
import Control.Monad
import Data.Maybe
import Data.Generics.Basics
import Data.Generics.Aliases
import Text.ParserCombinators.ReadP

------------------------------------------------------------------------------


-- | Generic show: an alternative to \"deriving Show\"
gshow :: Data a => a -> String

-- This is a prefix-show using surrounding "(" and ")",
-- where we recurse into subterms with gmapQ.
-- 
gshow = ( \t ->
                "("
             ++ showConstr (toConstr t)
             ++ concat (gmapQ ((++) " " . gshow) t)
             ++ ")"
        ) `extQ` (show :: String -> String)



-- | Generic read: an alternative to \"deriving Read\"
gread :: Data a => ReadS a

{-

This is a read operation which insists on prefix notation.  (The
Haskell 98 read deals with infix operators subject to associativity
and precedence as well.) We use fromConstrM to "parse" the input. To be
precise, fromConstrM is used for all types except String. The
type-specific case for String uses basic String read.

-}

gread = readP_to_S gread'

 where

  -- Helper for recursive read
  gread' :: Data a' => ReadP a'
  gread' = allButString `extR` stringCase

   where

    -- A specific case for strings
    stringCase :: ReadP String
    stringCase = readS_to_P reads

    -- Determine result type
    myDataType = dataTypeOf (getArg allButString)
     where
      getArg :: ReadP a'' -> a''
      getArg = undefined

    -- The generic default for gread
    allButString =
      do
 		-- Drop "  (  "
         skipSpaces			-- Discard leading space
         char '('			-- Parse '('
         skipSpaces			-- Discard following space

		-- Do the real work
	 str  <- parseConstr		-- Get a lexeme for the constructor
         con  <- str2con str		-- Convert it to a Constr (may fail)
         x    <- fromConstrM gread' con -- Read the children

		-- Drop "  )  "
         skipSpaces			-- Discard leading space
         char ')'			-- Parse ')'
         skipSpaces			-- Discard following space

         return x

    -- Turn string into constructor driven by the requested result type,
    -- failing in the monad if it isn't a constructor of this data type
    str2con :: String -> ReadP Constr	
    str2con = maybe mzero return
            . readConstr myDataType

    -- Get a Constr's string at the front of an input string
    parseConstr :: ReadP String
    parseConstr =  
               string "[]"     -- Compound lexeme "[]"
          <++  infixOp	       -- Infix operator in parantheses
          <++  readS_to_P lex  -- Ordinary constructors and literals

    -- Handle infix operators such as (:)
    infixOp :: ReadP String
    infixOp = do c1  <- char '('
                 str <- munch1 (not . (==) ')')
	         c2  <- char ')'
                 return $ [c1] ++ str ++ [c2]
