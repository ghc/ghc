% -----------------------------------------------------------------------------
% $Id: Ix.lhs,v 1.19 2001/08/29 09:34:05 simonmar Exp $
%
% (c) The University of Glasgow, 1994-2000
%

\section[Ix]{Module @Ix@}

\begin{code}
module Ix 
    (
	Ix
	  ( range	-- :: (Ix a) => (a,a) -> [a]
	  , index       -- :: (Ix a) => (a,a) -> a   -> Int
	  , inRange     -- :: (Ix a) => (a,a) -> a   -> Bool
	  , rangeSize	-- :: (Ix a) => (a,a) -> Int
	  )
    -- Ix instances:
    --
    --  Ix Char
    --  Ix Int
    --  Ix Integer
    --  Ix Bool
    --  Ix Ordering
    --  Ix ()
    --  (Ix a, Ix b) => Ix (a, b)
    --  ...

    -- Implementation checked wrt. Haskell 98 lib report, 1/99.
    ) where

import Prelude
#ifndef __HUGS__
import PrelArr
#endif
-- This module is empty, because Ix is defined in PrelArr.
-- Reason: it's needed internally in the Prelude.  
-- This module serves solely to export it to the user.

\end{code}

