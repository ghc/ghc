% ------------------------------------------------------------------------------
% $Id: Main.lhs,v 1.1 2001/06/28 14:15:03 simonmar Exp $
%
% (c) The University of Glasgow, 1994-2000
%

\section[GHC.Main]{Module @GHC.Main@}

\begin{code}
module GHC.Main( mainIO ) where

import {-# SOURCE #-} qualified Main	-- for type of "Main.main"

import Prelude

import System.IO
import GHC.Exception
import GHC.TopHandler

mainIO :: IO ()		-- It must be of type (IO t) because that's what
			-- the RTS expects.  GHC doesn't check this, so
			-- make sure this type signature stays!
mainIO = catchException Main.main topHandler
\end{code}
