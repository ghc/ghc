-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Text
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
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


import Control.Monad
import Data.Maybe
import Data.Typeable
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
             ++ conString (toConstr t)
             ++ concat (gmapQ ((++) " " . gshow) t)
             ++ ")"
        ) `extQ` (show :: String -> String)



-- | Generic read: an alternative to \"deriving Read\"
gread :: Data a => ReadS a

{-

This is a read operation which insists on prefix notation.  (The
Haskell 98 read deals with infix operators subject to associativity
and precedence as well.) We use gunfoldR to "parse" the input. To be
precise, gunfoldR is used for all types except String. The
type-specific case for String uses basic String read.

-}

gread = readP_to_S gread'

 where

  gread' :: Data a => ReadP a
  gread' = gdefault `extR` scase


   where

    -- A specific case for strings
    scase :: ReadP String
    scase = readS_to_P reads


    -- The generic default for gread
    -- gdefault :: Data a => ReadP a
    gdefault =
      do
		-- Drop "  (  "
         skipSpaces			-- Discard leading space
         char '('			-- Parse '('
         skipSpaces			-- Discard following space

		-- Do the real work
	 str   <- parseConstr		-- Get a lexeme for the constructor
         con   <- str2con str		-- Convert it to a Constr (may fail)
         x     <- gunfoldR con gread'	-- Read the children

		-- Drop "  )  "
         skipSpaces			-- Discard leading space
         char ')'			-- Parse ')'
         skipSpaces			-- Discard following space

         return x

     where

	-- Get the datatype for the type at hand;
	-- use gdefault to provide the type at hand.
	myDataTypeOf :: Data a => ReadP a -> DataType
	myDataTypeOf (_::ReadP a) = dataTypeOf (undefined::a)

	-- Turn string into constructor driven by gdefault's type,
	-- failing in the monad if it isn't a constructor of this data type
	str2con :: String -> ReadP Constr	
	str2con = maybe mzero return
                . stringCon (myDataTypeOf gdefault)

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
