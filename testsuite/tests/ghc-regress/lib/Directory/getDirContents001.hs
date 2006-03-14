import Directory (getDirectoryContents)
import List (sort, isPrefixOf, isSuffixOf)

main = do
    names <- getDirectoryContents "."
    putStrLn (unlines (sort (filter ok names)))

ok name = "getDirContents" `isPrefixOf` name 
	  && not ("bak" `isSuffixOf` name)
	  && not ("prof" `isSuffixOf` name)
	  && not ("hp" `isSuffixOf` name)
