\section{Life2}

\begin{code}
module Life2 (life2) where 
import UTypes
import UCopy (copy_FI)

life2 itLimit boardSize
 = (fBStr firstBoard) ++ (fBStr secondBoard)
   where {- ... -}

\end{code}

\begin{code}
   fBStr :: FI -> String
   fBStr FIN = []

{- OK
   firstBoard :: FI
   firstBoard = copy_FI boardSize (case 0 of
                                    (MkInt x) -> x)
-}

{- not happy about this -}
   
   firstBoard = copy_FI boardSize u0
   u0 = unBoxInt 0
   unBoxInt (MkInt x) = x
{- end of not happy -}

{- not happy with this either! -}

   secondBoard = copy_FI boardSize u1

   (MkInt u1) = 0
{- end of second not happy -}
\end{code}


