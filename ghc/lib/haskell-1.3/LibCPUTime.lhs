%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[LibCPUTime]{Haskell 1.3 CPU Time Library}

\begin{code}
module LibCPUTime where

import PreludeGlaST

getCPUTime :: IO Integer
getCPUTime =
    _ccall_ getCPUTime				    `thenPrimIO` \ ptr@(A# ptr#) ->
    if ptr /= ``NULL'' then
        return (fromInt (I# (indexIntOffAddr# ptr# 0#)) * 1000000000 + 
                fromInt (I# (indexIntOffAddr# ptr# 1#)) + 
		fromInt (I# (indexIntOffAddr# ptr# 2#)) * 1000000000 + 
                fromInt (I# (indexIntOffAddr# ptr# 3#)))
    else
	failWith (UnsupportedOperation "can't get CPU time")

\end{code}

Computation $getCPUTime$ returns the number of nanoseconds CPU time
used by the current program.  The precision of this result is
implementation-dependent.





