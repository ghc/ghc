% ------------------------------------------------------------------------------
% $Id: PrelMain.lhs,v 1.7 2000/06/30 13:39:36 simonmar Exp $
%
% (c) The University of Glasgow, 1994-2000
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
