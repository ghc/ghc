This is the mainPrimIO that must be used for Haskell~1.2.

\begin{code}
module Main ( mainPrimIO ) where

import PreludeMainIO_help	-- for type of "Main.main"
import PreludeDialogueIO	( requestToPrimIO )
import TyIO
import UTypes			( Bin )

mainPrimIO :: PrimIO ()
mainPrimIO s = case (requestToPrimIO main s) of
		( (), s2@(S# _) ) -> ( (), s2 )
\end{code}

OLD COMMENT:

Nota Bene!  @mainIO@ is written as an explicit function, rather than
by saying: @mainIO = requestToIO main@ so that the code generator
recognises @mainIO@ as a {\em function} (hence HNF, hence not
updatable), rather than a zero-arity CAF (hence updatable).  If it is
updated, then we have a mega-space leak, because the entire action
(@requestToIO main@) is retained indefinitely.

(This doesn't waste work because @mainIO@ is only used once.)
