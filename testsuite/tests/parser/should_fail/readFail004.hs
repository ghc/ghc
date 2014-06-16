-- !!! string gaps
-- !!!
module Main(main) where

-----------

main = putStr "\

\Some girls give me money\n\

\Some girls buy me clothes\n\

\..."

-----------

main2 = putStr "\
\ \
..."

-----------

main3 = putStr "\

\Some girls give me money\n\
-- and here is a comment
\Some girls buy me clothes\n\

\..."

-----------

main3 = putStr "\
{-
    and here is a nested {- comment -}
-}
\Some girls give me money\n\

\Some girls buy me clothes\n\

\..."
