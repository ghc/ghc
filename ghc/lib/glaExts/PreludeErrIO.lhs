%
% (c) The GRASP/AQUA Project, Glasgow University, 1993
%
\section[PreludeErrIO]{Wrapper for errorIO primitive}

The boxified version of the @errorIO#@ primitive.

\begin{code}
module PreludeErrIO where

errorIO :: PrimIO () -> a

errorIO io
  = case (errorIO# io) of
      _ -> bottom
  where
    bottom = bottom		-- Never evaluated
\end{code}
