import Directory (getDirectoryContents)
import List (sort, isPrefixOf, isSuffixOf)

main = do
    names <- getDirectoryContents "."
    putStrLn (unlines (sort (filter ok names)))

ok name = "getDirectoryContents" `isPrefixOf` name 
	  && not ("bak" `isSuffixOf` name)
