\section{AbsCg} 
 
This module imports definitions from Cg and
renames them before exporting to Main.

Matrix and AbsDensematrix are imported to keep hbc happy.

\begin{code}

module AbsCg (solve_iters, Cg_state(..), show_state) where

import Cg
import Matrix
import AbsDensematrix

solve_iters = cgiters

show_state = showcg_state

\end{code}

