% -----------------------------------------------------------------------------
% $Id: Stable.lhs,v 1.1 1999/01/26 12:24:58 simonm Exp $
%
% (c) The GHC Team, 1999
%

\section[Stable]{Module @Stable@}

\begin{code}
module Stable

       	( StableName {-a-}	-- abstract.
	, makeStableName	-- :: a -> IO (StableName a)
	, hashStableName	-- :: StableName a -> Int

	, StablePtr {-a-} 	-- abstract.
	, makeStablePtr  	-- :: a -> IO (StablePtr a)
	, deRefStablePtr  	-- :: StablePtr a -> IO a
	, freeStablePtr   	-- :: StablePtr a -> IO ()
	)

  where

import PrelBase
import PrelIOBase
import PrelStable

-----------------------------------------------------------------------------
-- Stable Names

data StableName a = StableName (StableName# a)

makeStableName  :: a -> IO (StableName a)
hashStableName :: StableName a -> Int

makeStableName a = IO $ \ s ->
    case makeStableName# a s of (# s', sn #) -> (# s', StableName sn #)

hashStableName (StableName sn) = I# (stableNameToInt# sn)

instance Eq (StableName a) where 
    (StableName sn1) == (StableName sn2) = 
       case eqStableName# sn1 sn2 of
	 0# -> False
	 _  -> True

\end{code}
