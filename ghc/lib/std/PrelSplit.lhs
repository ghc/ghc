\begin{code}
module PrelSplit( Splittable( split ) ) where

-- The Splittable class for the linear implicit parameters
-- Can't put it in PrelBase, because of the use of (,)

class Splittable t where
  split :: t -> (t,t)
\end{code}
