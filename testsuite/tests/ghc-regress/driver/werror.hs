{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-- check that all warnings are emitted before failing due to -Werror (#1666)

-- missing type sig warning (type checker)
main = do
        let main = main -- shadowing warning (renamer)
	putStrLn "hello" -- tab warning (lexer)

f [] = []
f [] = [] -- overlapping pattern
-- incomplete pattern

