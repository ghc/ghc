%
% (c) The AQUA Project, Glasgow University, 1994-1997
%

\section[GHCmain]{Module @GHCmain@}

\begin{code}
module GHCmain( mainIO ) where

import Prelude
import {-# SOURCE #-} qualified Main	-- for type of "Main.main"
\end{code}

\begin{code}
mainIO :: IO ()		-- It must be of type (IO t) because that's what
			-- the RTS expects.  GHC doesn't check this, so
			-- make sure this type signature stays!
mainIO = catch Main.main (\err -> error ("I/O error: "++showsPrec 0 err "\n"))
\end{code}
