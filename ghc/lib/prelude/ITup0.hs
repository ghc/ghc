module PreludeBuiltin where

--- 0-tuple (trivial type "()" ) ---------------------------------

import Cls
import Core
import IChar
import IList
import List		( (++), foldr )
import PS		( _PackedString, _unpackPS )
import Text

instance Eq () where
    () == () = True
    () /= () = False

instance Ord () where
    () <= () = True
    () <  () = False
    () >= () = True
    () >  () = False
    max () () = ()
    min () () = ()
    _tagCmp () () = _EQ

instance Text () where
    readsPrec p    = readParen False
    	    	    	    (\r -> [((),t) | ("(",s) <- lex r,
					     (")",t) <- lex s ] )
    showsPrec p () = showString "()"

instance Ix () where
    range   ((), ())    = [()]
    index   ((), ()) () = 0
    inRange ((), ()) () = True

instance Enum () where
    enumFrom () 	= [()]
    enumFromThen () () 	= [()]
    enumFromTo () () 	= [()]
    enumFromThenTo () () () = [()]

-- ToDo: something for Binary
