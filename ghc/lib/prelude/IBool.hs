module PreludeCore ( Bool(..) ) where

import Prel		( (&&) )
import Core
import Cls
import IChar
import IInt
import IList
import List		( (++), map, foldr )
import PS		( _PackedString, _unpackPS )
import Text

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

----------------------------------------------------------------------
instance Text Bool where
    readsPrec p
      = readParen (p > 9)
            (\ b -> [ (False, c) | ("False", c) <- lex b ]
		 ++ [ (True,  c) | ("True",  c) <- lex b ])

    showsPrec d p r = (if p then "True" else "False") ++ r

-- ToDo: Binary
