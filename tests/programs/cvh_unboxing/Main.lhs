\begin{code}
{-# LANGUAGE MagicHash #-}

import GHC.Exts
import Types
import Append

main = putStr (show (append_FC_L_L (FC2 a_ a_) []))
   where a_ = case 'a' of { C# x -> x }
\end{code}
