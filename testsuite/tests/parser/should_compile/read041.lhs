-- Tests trac #210.

module ShouldCompile where

\begin{code}
  main = foo
\end{code}

> foo = putStrLn "Foo"

