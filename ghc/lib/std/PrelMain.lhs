%
% (c) The AQUA Project, Glasgow University, 1994-1997
%

\section[PrelMain]{Module @PrelMain@}

\begin{code}
module PrelMain( mainIO ) where

import Prelude
import {-# SOURCE #-} qualified Main	-- for type of "Main.main"

import PrelException
import PrelHandle ( topHandler )

\end{code}

\begin{code}
mainIO :: IO ()		-- It must be of type (IO t) because that's what
			-- the RTS expects.  GHC doesn't check this, so
			-- make sure this type signature stays!
mainIO = catchException Main.main (topHandler True)
\end{code}
