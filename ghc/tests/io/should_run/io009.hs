import Directory (getDirectoryContents)
import List (sort, isPrefixOf)

main = do
    names <- getDirectoryContents "."
    let names' = filter (isPrefixOf "io009") names
    putStrLn (unlines (sort names'))
