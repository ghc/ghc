This is the mainPrimIO13 that must be used for Haskell~1.3.

\begin{code}
module Main ( mainPrimIO13 ) where

import PreludeMain13_help	-- for type of "Main.main"
import Builtin			( error )
import PreludeIO
import UTypes			( Bin )

import Cls
import Core
import IChar
import IInt
import IList
import List		( (++) )
import Prel		( (.), not )
import PS		( _PackedString, _unpackPS )
import Text
import TyComplex
import TyArray

mainPrimIO13 :: PrimIO ()

mainPrimIO13 s
  = case (main s) of { (result, s2@(S# _)) ->
    case result   of
      Right ()  -> ( (), s2 )
      Left  err -> error ("I/O error: "++showsPrec 0 err "\n")
    }
\end{code}

OLD COMMENT:

Nota Bene!  @mainIO@ is written as an explicit function, rather than
by saying: @mainIO = requestToIO main@ so that the code generator
recognises @mainIO@ as a {\em function} (hence HNF, hence not
updatable), rather than a zero-arity CAF (hence updatable).  If it is
updated, then we have a mega-space leak, because the entire action
(@requestToIO main@) is retained indefinitely.

(This doesn't waste work because @mainIO@ is only used once.)
