module PreludeCore ( Bool(..) ) where

import Prel		( (&&) )
import Core
import Cls
import IChar
import IInt
import IList
import List		( (++), map, foldr, takeWhile )
import PS		( _PackedString, _unpackPS )
import Text
import TyArray
import TyComplex

----------------------------------------------------------------------
instance Eq Bool where
    True  == True	= True
    False == False	= True
    _     == _	    	= False

    True  /= False	= True
    False /= True	= True
    _     /= _	    	= False

----------------------------------------------------------------------
instance Ord Bool where
    a <  b  = case _tagCmp a b of { _LT -> True;  _EQ -> False; _GT -> False }
    a <= b  = case _tagCmp a b of { _LT -> True;  _EQ -> True;  _GT -> False }
    a >= b  = case _tagCmp a b of { _LT -> False; _EQ -> True;  _GT -> True  }
    a >  b  = case _tagCmp a b of { _LT -> False; _EQ -> False; _GT -> True  }

    max a b = case _tagCmp a b of { _LT -> b; _EQ -> a;  _GT -> a }
    min a b = case _tagCmp a b of { _LT -> a; _EQ -> a;  _GT -> b }

    _tagCmp True  True	= _EQ
    _tagCmp True	 False	= _GT
    _tagCmp False True	= _LT
    _tagCmp False False	= _EQ

----------------------------------------------------------------------
instance Ix Bool where
    range   (l,u)   = map bdecode [(bencode l) .. (bencode u)]
    index   (l,u) i = (bencode i) - (bencode l)
    inRange (l,u) i = (bencode i) >= (bencode l) && (bencode i) <= (bencode u)

bencode :: Bool -> Int
bencode False = 0
bencode True  = 1

bdecode :: Int -> Bool
bdecode b = if	    b == 0 then False
	    else if b == 1 then True
	    else error "decode Bool\n"

----------------------------------------------------------------------
instance Enum Bool where
    enumFrom False	=  [False, True]
    enumFrom True	=  [True]

    enumFromThen False True   = [False, True]
    enumFromThen True  False  = [True, False]
    enumFromThen b     _      = bs where bs = b : bs

    enumFromTo n m       =  takeWhile (<= m) (enumFrom n)
    enumFromThenTo n m p =  takeWhile (if m >= n then (<= p) else (>= p))
				      (enumFromThen n m)

----------------------------------------------------------------------
instance Text Bool where
    readsPrec p r
      = readParen False (\ b -> [ (False, c) | ("False", c) <- lex b ]) r
     ++ readParen False (\ b -> [ (True,  c) | ("True",  c) <- lex b ]) r

    showsPrec d p = showString (if p then "True" else "False")

    readList = _readList (readsPrec 0)
    showList = _showList (showsPrec 0) 

-- ToDo: Binary
