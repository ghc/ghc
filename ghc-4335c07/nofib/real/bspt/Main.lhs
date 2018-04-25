\begin{code}
module Main where
import Prog (prog)

main = do
    str <- getContents
    putStr (prog str)
\end{code}
