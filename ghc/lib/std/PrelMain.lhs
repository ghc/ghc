% ------------------------------------------------------------------------------
% $Id: PrelMain.lhs,v 1.9 2001/05/21 14:07:31 simonmar Exp $
%
% (c) The University of Glasgow, 1994-2000
%

\section[PrelMain]{Module @PrelMain@}

\begin{code}
module PrelMain( mainIO ) where

import {-# SOURCE #-} qualified Main	-- for type of "Main.main"

import IO
import PrelException
import PrelTopHandler

mainIO :: IO ()		-- It must be of type (IO t) because that's what
			-- the RTS expects.  GHC doesn't check this, so
			-- make sure this type signature stays!
mainIO = catchException Main.main topHandler
\end{code}
