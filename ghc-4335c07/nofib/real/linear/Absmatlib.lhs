\section{Absmatlib}

This module exports definitions imported from
Matlib. Matrix and AbsDensematrix are imported
to keep hbc happy

\begin{code}

module Absmatlib(doscale,doprecond,uncondition) where

import Matlib
import Matrix -- for hbc
import AbsDensematrix -- for hbc


doscale = scale
doprecond = precond

\end{code}


