\section[GHCmain]{Module @GHCmain@}

This is the mainPrimIO that must be used for Haskell~1.3.

\begin{code}
module GHCmain( mainPrimIO ) where

import Prelude
import {-# SOURCE #-} qualified Main	-- for type of "Main.main"
import IOBase
import STBase
\end{code}

\begin{code}
mainPrimIO = ST $ \ s ->
    case Main.main   of { IO (ST main_guts) ->
    case main_guts s of { (result, s2@(S# _)) ->
    case result   of
      Right ()  -> ( (), s2 )
      Left  err -> error ("I/O error: "++showsPrec 0 err "\n")
    }}
\end{code}

OLD COMMENT:

Nota Bene!  @mainIO@ is written as an explicit function, rather than
by saying: @mainIO = requestToIO main@ so that the code generator
recognises @mainIO@ as a {\em function} (hence HNF, hence not
updatable), rather than a zero-arity CAF (hence updatable).  If it is
updated, then we have a mega-space leak, because the entire action
(@requestToIO main@) is retained indefinitely.

(This doesn't waste work because @mainIO@ is only used once.)
