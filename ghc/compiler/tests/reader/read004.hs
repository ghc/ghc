--!!! string gaps
--!!!

-----------

main = appendChan stdout "\

\Some girls give me money\n\

\Some girls buy me clothes\n\

\..."
	  exit done

-----------

main2 = appendChan stdout "\
\ \
..." exit done

-----------

main3 = appendChan stdout "\

\Some girls give me money\n\
-- and here is a comment
\Some girls buy me clothes\n\

\..."
	  exit done

-----------

main3 = appendChan stdout "\
{-
    and here is a nested {- comment -}
-}
\Some girls give me money\n\

\Some girls buy me clothes\n\

\..."
	  exit done
