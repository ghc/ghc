%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[LibCPUTime]{Haskell 1.3 CPU Time Library}

\begin{code}
module LibCPUTime where

import PreludeGlaST

getCPUTime :: IO Integer
getCPUTime =
    newIntArray (0,3)				    `thenPrimIO` \ marr ->
    unsafeFreezeByteArray marr			    `thenPrimIO` \ barr@(_ByteArray _ frozen#) ->
    _ccall_ getCPUTime barr			    `thenPrimIO` \ ptr ->
    if (ptr::_Addr) /= ``NULL'' then
        return (fromInt (I# (indexIntArray# frozen# 0#)) * 1000000000 + 
                fromInt (I# (indexIntArray# frozen# 1#)) + 
		fromInt (I# (indexIntArray# frozen# 2#)) * 1000000000 + 
                fromInt (I# (indexIntArray# frozen# 3#)))
    else
	failWith (UnsupportedOperation "can't get CPU time")

\end{code}

Computation $getCPUTime$ returns the number of nanoseconds CPU time
used by the current program.  The precision of this result is
implementation-dependent.





 
