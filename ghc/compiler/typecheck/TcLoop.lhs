This module breaks the loops among the typechecker modules
TcExpr, TcBinds, TcMonoBnds, TcQuals, TcGRHSs, TcMatches.

\begin{code}
module TcLoop( tcGRHSsAndBinds )
import TcGRHSs( tcGRHSsAndBinds )
\end{code}
